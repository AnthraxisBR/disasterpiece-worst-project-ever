#!/bin/bash

# Cleanup and stop the container

cleanup_container() {
    log "Cleaning up cage-container..."
    
    # Clean up port forwarding first
    if [[ -f "$SCRIPT_DIR/lib/setup_port_forward.sh" ]]; then
        source "$SCRIPT_DIR/lib/setup_port_forward.sh"
        cleanup_port_forwarding
    fi
    
    # Get container PID from file
    local container_pid=$(get_container_pid)
    
    if [[ -n "$container_pid" ]]; then
        log "Stopping container process ($container_pid)"
        
        # Check if process actually exists
        if kill -0 "$container_pid" 2>/dev/null; then
            # Send SIGTERM to container process
            kill -SIGTERM "$container_pid" 2>/dev/null || true
            
            # Give it some time to terminate gracefully
            sleep 2
            
            # Kill all processes in the container namespace if still running
            if kill -0 "$container_pid" 2>/dev/null; then
                log "Container still running, sending SIGKILL"
                kill -9 "$container_pid" 2>/dev/null || true
                
                # Kill any remaining processes inside the container
                if command -v nsenter &> /dev/null; then
                    log "Killing all processes in container namespace"
                    nsenter --target "$container_pid" --pid kill -9 -1 2>/dev/null || true
                fi
                
                # Last resort - find and kill all processes with ppid of container
                log "Finding and killing all child processes"
                pkill -P "$container_pid" 2>/dev/null || true
            fi
        else
            warn "Container process $container_pid no longer exists"
        fi
        
        # Remove PID file
        rm -f "$CONTAINER_PID_FILE"
    else
        warn "No container PID found, container may not be running"
    fi
    
    # Clean up cgroups
    cleanup_cgroups
    
    # Clean up network
    cleanup_network
    
    # Clean up mounts
    cleanup_mounts
    
    # Create an empty log to indicate successful cleanup
    touch "$SCRIPT_DIR/logs/last_cleanup_success"
    
    success "Container cleanup completed"
}

# Clean up cgroups
cleanup_cgroups() {
    log "Cleaning up simulated cgroups"
    
    # For simulated cgroups, we just need to unmount the tmpfs
    if [[ -d "$CGROUP_PATH" ]]; then
        # Clean individual cgroup controllers to be thorough
        for controller in cpu memory pids; do
            log "Cleaning $controller cgroup"
            
            # Remove the content from cgroup.procs file (just for cleanup)
            if [[ -f "$CGROUP_PATH/$controller/cage-container/cgroup.procs" ]]; then
                echo "" > "$CGROUP_PATH/$controller/cage-container/cgroup.procs" 2>/dev/null || true
            fi
        done
        
        # Unmount the tmpfs if mounted
        if mount | grep -q "$CGROUP_PATH"; then
            umount -f "$CGROUP_PATH" 2>/dev/null || 
                umount -l "$CGROUP_PATH" 2>/dev/null || 
                warn "Failed to unmount simulated cgroup filesystem"
        fi
    fi
}

# Clean up network
cleanup_network() {
    log "Cleaning up network"
    
    # Determine IP and subnet info for cleanup
    local ip_info=""
    if [[ -n "$IP_ONLY" && -n "$SUBNET_MASK" ]]; then
        ip_info="$IP_ONLY/$SUBNET_MASK"
        log "Using network info from config: $ip_info"
    else
        # Try to get network info from interface if config is missing
        ip_info=$(ip -brief addr show "$NET_INTERFACE" 2>/dev/null | awk '{print $3}' | head -1)
        if [[ -n "$ip_info" ]]; then
            log "Found network info from interface: $ip_info"
        else
            warn "No network info available for cleanup"
        fi
    fi
    
    # Remove network namespace
    if ip netns list 2>/dev/null | grep -q "$NET_NAMESPACE"; then
        log "Removing network namespace: $NET_NAMESPACE"
        ip netns del "$NET_NAMESPACE" 2>/dev/null || warn "Failed to delete network namespace"
    fi
    
    # Remove veth interface
    if ip link show "$NET_INTERFACE" &> /dev/null; then
        log "Removing veth interface: $NET_INTERFACE"
        ip link del "$NET_INTERFACE" 2>/dev/null || warn "Failed to delete veth interface"
    fi
    
    # Clean up iptables rules
    if command -v iptables &> /dev/null && [[ -n "$ip_info" ]]; then
        log "Cleaning up iptables rules"
        
        # Clean up any NAT rules for this container
        iptables -t nat -D POSTROUTING -s "$ip_info" -j MASQUERADE 2>/dev/null || true
        
        # Clean up any forwarding rules for this container
        for interface in "$NET_INTERFACE" veth0 eth0; do
            iptables -D FORWARD -i "$interface" -j ACCEPT 2>/dev/null || true
            iptables -D FORWARD -o "$interface" -j ACCEPT 2>/dev/null || true
        done
        
        # Check if port forwarding was set up
        if [[ -f "$SCRIPT_DIR/logs/port_forward.info" ]]; then
            local port_info=$(cat "$SCRIPT_DIR/logs/port_forward.info")
            local host_port=$(echo "$port_info" | cut -d':' -f1)
            if [[ -n "$host_port" ]]; then
                log "Cleaning up port forwarding for port $host_port"
                iptables -t nat -D PREROUTING -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
                iptables -t nat -D OUTPUT -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
            fi
        fi
    fi
}

# Clean up mounts
cleanup_mounts() {
    log "Cleaning up mounts"
    
    # Check if squashfs is mounted
    if mount | grep -q "/mnt/cage-container"; then
        log "Unmounting SquashFS image"
        umount -f /mnt/cage-container 2>/dev/null || 
            warn "Failed to unmount SquashFS image, will try lazy unmount"
        
        # Try lazy unmount if normal unmount fails
        if mount | grep -q "/mnt/cage-container"; then
            umount -l /mnt/cage-container 2>/dev/null || 
                warn "Failed to lazy unmount SquashFS image"
        fi
    fi
    
    # Unmount rootfs proc
    if [[ -d "$ROOTFS/proc" ]] && mount | grep -q "$ROOTFS/proc"; then
        umount -f "$ROOTFS/proc" 2>/dev/null || 
            umount -l "$ROOTFS/proc" 2>/dev/null || 
            warn "Failed to unmount rootfs proc"
    fi
    
    # Unmount rootfs sys
    if [[ -d "$ROOTFS/sys" ]] && mount | grep -q "$ROOTFS/sys"; then
        umount -f "$ROOTFS/sys" 2>/dev/null || 
            umount -l "$ROOTFS/sys" 2>/dev/null || 
            warn "Failed to unmount rootfs sys"
    fi
    
    # Unmount rootfs cgroups
    for cgroup_path in "$CGROUP_PATH" "$ROOTFS/sys/fs/cgroup"; do
        if [[ -d "$cgroup_path" ]]; then
            for subsys in cpu memory pids; do
                if [[ -d "$cgroup_path/$subsys" ]] && mount | grep -q "$cgroup_path/$subsys"; then
                    umount -f "$cgroup_path/$subsys" 2>/dev/null || 
                        umount -l "$cgroup_path/$subsys" 2>/dev/null || 
                        warn "Failed to unmount $subsys cgroup"
                fi
            done
            
            # Unmount the main cgroup filesystem
            if mount | grep -q "$cgroup_path"; then
                umount -f "$cgroup_path" 2>/dev/null || 
                    umount -l "$cgroup_path" 2>/dev/null || 
                    warn "Failed to unmount cgroup filesystem"
            fi
        fi
    done
    
    # Clean up loop devices
    if losetup | grep -q "cage-container.img"; then
        log "Detaching loop device for cage-container.img"
        losetup -d $(losetup | grep cage-container.img | awk '{print $1}') 2>/dev/null || 
            warn "Failed to detach loop device"
    fi
    
    # Remove squashfs image
    if [[ -f "/var/cage-container.img" ]]; then
        log "Removing SquashFS image"
        rm -f /var/cage-container.img 2>/dev/null || 
            warn "Failed to remove SquashFS image (may be in use)"
    fi
}

# Export cleanup function (called by trap in main script)
export -f cleanup_container 