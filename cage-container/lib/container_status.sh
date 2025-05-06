#!/bin/bash

# Script to check and display status of running containers

get_container_pid() {
    if [[ -f "$CONTAINER_PID_FILE" ]]; then
        cat "$CONTAINER_PID_FILE"
    else
        echo "0"
    fi
}

# Get status of container
get_container_status() {
    local container_pid=$(get_container_pid)
    
    if [[ -z "$container_pid" ]]; then
        error "No container is running (no PID file found)"
        return 1
    fi
    
    # Check if the process is actually running
    if ! kill -0 "$container_pid" 2>/dev/null; then
        error "Container process $container_pid exists in PID file but is not running"
        log "You may need to clean up with: $0 --stop"
        # Automatically remove stale PID file
        rm -f "$CONTAINER_PID_FILE"
        return 1
    fi
    
    # Print container info
    success "Container is running with PID: $container_pid"
    
    # Get uptime
    local start_time=$(ps -o lstart= -p "$container_pid" 2>/dev/null)
    if [[ -n "$start_time" ]]; then
        log "Started at: $start_time"
    fi
    
    # Show resource usage
    show_resource_usage "$container_pid"
    
    # Show network info
    show_network_info
    
    return 0
}

# Show resource usage
show_resource_usage() {
    local pid="$1"
    
    log "Resource usage:"
    
    # Memory usage
    local mem_usage=$(ps -o rss= -p "$pid" 2>/dev/null)
    if [[ -n "$mem_usage" ]]; then
        log "  Memory: $((mem_usage / 1024)) MB"
    else
        warn "  Memory usage information not available"
    fi
    
    # CPU usage
    local cpu_usage=$(ps -o %cpu= -p "$pid" 2>/dev/null)
    if [[ -n "$cpu_usage" ]]; then
        log "  CPU: ${cpu_usage}%"
    else
        warn "  CPU usage information not available"
    fi
    
    # Get cgroup limits if accessible - make this more robust for different environments
    if [[ -d "$CGROUP_PATH" ]]; then
        # Memory limit - handle both v1 and v2 cgroup paths
        for path in "$CGROUP_PATH/memory/cage-container/memory.limit_in_bytes" \
                   "$CGROUP_PATH/memory.max" \
                   "/sys/fs/cgroup/memory/cage-container/memory.limit_in_bytes"; do
            if [[ -f "$path" ]]; then
                local mem_limit=$(cat "$path" 2>/dev/null)
                if [[ -n "$mem_limit" && "$mem_limit" != "max" ]]; then
                    log "  Memory limit: $((mem_limit / 1024 / 1024)) MB"
                    break
                fi
            fi
        done
        
        # CPU limit - handle both v1 and v2 cgroup paths
        for path in "$CGROUP_PATH/cpu/cage-container/cpu.cfs_quota_us" \
                   "$CGROUP_PATH/cpu.max" \
                   "/sys/fs/cgroup/cpu/cage-container/cpu.cfs_quota_us"; do
            if [[ -f "$path" ]]; then
                local cpu_quota=$(cat "$path" 2>/dev/null)
                local cpu_period=$(cat "${path%/*}/cpu.cfs_period_us" 2>/dev/null || echo 100000)
                if [[ -n "$cpu_quota" && "$cpu_quota" != -1 && "$cpu_quota" != "max" ]]; then
                    log "  CPU limit: $((cpu_quota * 100 / cpu_period))%"
                    break
                fi
            fi
        done
        
        # Process count - handle both v1 and v2 cgroup paths
        for path in "$CGROUP_PATH/pids/cage-container/pids.current" \
                   "$CGROUP_PATH/pids.current" \
                   "/sys/fs/cgroup/pids/cage-container/pids.current"; do
            if [[ -f "$path" ]]; then
                local pids_current=$(cat "$path" 2>/dev/null)
                if [[ -n "$pids_current" ]]; then
                    log "  Processes: $pids_current"
                    break
                fi
            fi
        done
    else
        warn "  Cgroup information not available (cgroup path not found)"
        # Try system cgroups directly as fallback
        if [[ -d "/sys/fs/cgroup" ]]; then
            log "  Checking system cgroups directly..."
            if [[ -f "/sys/fs/cgroup/memory/cage-container/memory.limit_in_bytes" ]]; then
                local mem_limit=$(cat "/sys/fs/cgroup/memory/cage-container/memory.limit_in_bytes" 2>/dev/null)
                if [[ -n "$mem_limit" ]]; then
                    log "  Memory limit: $((mem_limit / 1024 / 1024)) MB"
                fi
            fi
        fi
    fi
}

# Show network information
show_network_info() {
    if ! command -v ip &> /dev/null; then
        warn "ip command not found, skipping network info"
        return
    fi
    
    log "Network information:"
    
    # Check if namespace exists
    if ip netns list 2>/dev/null | grep -q "$NET_NAMESPACE"; then
        log "  Network namespace: $NET_NAMESPACE"
        
        # Show IP address more robustly
        local ip_info=$(ip netns exec "$NET_NAMESPACE" ip -brief addr show dev "$VETH_PEER" 2>/dev/null)
        if [[ -n "$ip_info" ]]; then
            log "  Interface: $VETH_PEER"
            log "  IP address: $(echo "$ip_info" | awk '{print $3}')"
        else
            # Try fallback without namespace exec
            ip_info=$(ip -brief addr show dev "$NET_INTERFACE" 2>/dev/null)
            if [[ -n "$ip_info" ]]; then
                log "  Host Interface: $NET_INTERFACE"
                log "  Host IP address: $(echo "$ip_info" | awk '{print $3}')"
            fi
        fi
        
        # Show listening ports in the namespace
        if command -v netstat &> /dev/null; then
            log "  Attempting to check listening ports..."
            local ports=$(ip netns exec "$NET_NAMESPACE" netstat -tulpn 2>/dev/null | grep LISTEN | awk '{print $4}' | cut -d: -f2)
            if [[ -n "$ports" ]]; then
                log "  Listening ports: $ports"
            else
                # Fallback to ss command
                if command -v ss &> /dev/null; then
                    ports=$(ip netns exec "$NET_NAMESPACE" ss -tulpn 2>/dev/null | grep LISTEN | awk '{print $5}' | cut -d: -f2)
                    if [[ -n "$ports" ]]; then
                        log "  Listening ports: $ports"
                    fi
                fi
            fi
        fi
    else
        warn "  Network namespace $NET_NAMESPACE not found"
        # Check if we have any veth interfaces
        if ip link show | grep -q veth; then
            log "  Found veth interfaces:"
            ip link show | grep veth | awk '{print $2}' | tr -d ':'
        fi
    fi
    
    # Show port forwarding info
    if [[ -f "$SCRIPT_DIR/logs/port_forward.info" ]]; then
        local port_info=$(cat "$SCRIPT_DIR/logs/port_forward.info")
        log "  Port forwarding: $port_info (host:container)"
        log "  Container should be accessible at: http://localhost:$(echo "$port_info" | cut -d':' -f1)"
    else
        warn "  No port forwarding information found"
        # Check for any socat or netcat processes
        if pgrep -f "socat.*:8080" > /dev/null; then
            log "  Found socat process forwarding port 8080"
        elif pgrep -f "nc.*:8080" > /dev/null; then
            log "  Found netcat process forwarding port 8080"
        fi
    fi
}

# List running processes in container
list_container_processes() {
    local container_pid=$(get_container_pid)
    
    if [[ -z "$container_pid" ]]; then
        error "No container is running (no PID file found)"
        return 1
    fi
    
    # Check if the process is actually running
    if ! kill -0 "$container_pid" 2>/dev/null; then
        error "Container process $container_pid exists in PID file but is not running"
        log "You may need to clean up with: $0 --stop"
        # Automatically remove stale PID file
        rm -f "$CONTAINER_PID_FILE"
        return 1
    fi
    
    log "Container processes:"
    
    # Try multiple methods to get processes
    if command -v nsenter &> /dev/null; then
        # Use nsenter to run ps in the container PID namespace
        log "Trying with nsenter..."
        nsenter --target "$container_pid" --pid ps -ef 2>/dev/null | grep -v '^root.*ps -ef' || {
            # Try with ps with process subtree
            log "Fallback: using process subtree..."
            if ps -p "$container_pid" > /dev/null 2>&1; then
                ps --forest -o pid,cmd -p "$container_pid" --deselect
                ps --forest -o pid,cmd --ppid "$container_pid"
            else
                # Last resort - check cgroup if available
                log "Fallback: checking cgroups..."
                for cgroup_procs in "$CGROUP_PATH/pids/cage-container/cgroup.procs" \
                                  "$CGROUP_PATH/cgroup.procs" \
                                  "/sys/fs/cgroup/pids/cage-container/cgroup.procs"; do
                    if [[ -f "$cgroup_procs" ]]; then
                        echo "PID   COMMAND"
                        cat "$cgroup_procs" 2>/dev/null | while read pid; do
                            if kill -0 "$pid" 2>/dev/null; then
                                echo "$pid   $(ps -o cmd= -p "$pid" 2>/dev/null || echo '[unknown]')"
                            fi
                        done
                        break
                    fi
                done
            fi
        }
    else
        warn "nsenter command not found, using basic process listing"
        ps f -p "$container_pid" -o pid,ppid,cmd
        ps --forest -o pid,ppid,cmd --ppid "$container_pid" 2>/dev/null
    fi
}

# Run container inspection
inspect_container() {
    get_container_status
    list_container_processes
} 