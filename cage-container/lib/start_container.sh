#!/bin/bash

# Start the container with proper isolation

start_container() {
    log "Starting container..."
    
    # Get applications from config
    get_applications_from_config
    
    # Check for existing network namespace
    if ! ip netns list 2>/dev/null | grep -q "$NET_NAMESPACE"; then
        error "Network namespace $NET_NAMESPACE does not exist. Network setup may have failed."
        error "Try running cage-container.sh --reset and then start again."
        exit 1
    fi
    
    # Start container in isolated namespaces - improved to handle WSL limitations
    log "Starting container with namespaces isolation"
    
    # Create a new directory for namespace references
    mkdir -p /var/run/cage-container
    
    # Log application command to help with debugging
    log "Application command to be executed: $APPLICATION_COMMAND"
    
    # Mount proc for the container - do this BEFORE starting the container since squashfs is read-only
    log "Mounting proc filesystem for the container"
    mkdir -p "/mnt/cage-container/proc" || log "proc directory already exists"
    mount -t proc proc "/mnt/cage-container/proc" || {
        warn "Failed to mount proc, trying to unmount first"
        umount -f "/mnt/cage-container/proc" 2>/dev/null || true
        mount -t proc proc "/mnt/cage-container/proc" || 
            warn "Failed to mount proc, container may have limited functionality"
    }
    
    # Create a more robust startup script that will be executed by unshare
    # This addresses problems with nested quoting and variables in WSL
    STARTUP_SCRIPT="/tmp/cage-container-startup-$$.sh"
    cat > "$STARTUP_SCRIPT" << 'EOFSCRIPT'
#!/bin/bash

# This script is executed by unshare to start the container
set -e

# Change root to the container filesystem
cd /mnt/cage-container && exec chroot . /bin/sh -c "
    # Set hostname
    hostname cage-container 2>/dev/null || echo cage-container > /etc/hostname
    
    # Setup dynamic library path
    export LD_LIBRARY_PATH=/usr/local/lib:/lib:/usr/lib:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu
    
    # Start cgroup monitor in background
    if [ -f /app/cgroup_monitor.sh ]; then
        chmod +x /app/cgroup_monitor.sh 2>/dev/null || true
        /app/cgroup_monitor.sh &
    fi
    
    # Ensure dev nodes are available
    if [ ! -c /dev/null ]; then
        mkdir -p /dev
        mknod -m 666 /dev/null c 1 3 2>/dev/null || echo 'Failed to create /dev/null'
    fi
    
    # Start the application (assuming it's the webserver)
    cd /app && ./run.sh
"
EOFSCRIPT
    
    # Make the script executable
    chmod +x "$STARTUP_SCRIPT"
    
    # Use unshare to create new namespaces - simpler approach with fewer nested quotes
    # Try a more compatible set of namespace flags for WSL
    # Adding --mount-proc to ensure /proc is always mounted
    unshare --mount --uts --pid --mount-proc --fork "$STARTUP_SCRIPT" &
    
    # Give processes a moment to start up
    sleep 2
    
    # Get the container PID - use pgrep to find child processes of unshare
    CONTAINER_PID=$(pgrep -f "unshare.*$STARTUP_SCRIPT" | head -1)
    
    if [[ -z "$CONTAINER_PID" ]]; then
        warn "Could not find container process, trying alternative method"
        # Alternative method to find the process
        CONTAINER_PID=$(ps aux | grep "$STARTUP_SCRIPT" | grep -v grep | awk '{print $2}' | head -1)
    fi
    
    if [[ -z "$CONTAINER_PID" ]]; then
        error "Failed to start container - could not determine container PID"
        log "Check logs for errors: $SCRIPT_DIR/logs/container.log"
        exit 1
    fi
    
    # Save container PID to file
    echo "$CONTAINER_PID" > "$CONTAINER_PID_FILE"
    log "Container PID file created at $CONTAINER_PID_FILE with PID: $CONTAINER_PID"
    
    # Add container to cgroups with improved error handling
    add_to_cgroups
    
    # Clean up the temporary script after some time to allow debugging if needed
    (sleep 10 && rm -f "$STARTUP_SCRIPT") &
    
    success "Container started with PID: $CONTAINER_PID"
}

# Add container to cgroups with improved error handling
add_to_cgroups() {
    log "Simulating cgroup assignment for container process ($CONTAINER_PID)"
    
    # Add to CPU cgroup - just write PID to the simulated file
    if [[ -f "$CGROUP_PATH/cpu/cage-container/cgroup.procs" ]]; then
        echo "$CONTAINER_PID" > "$CGROUP_PATH/cpu/cage-container/cgroup.procs" 2>/dev/null
        log "Added container to simulated CPU cgroup"
    fi
    
    # Add to memory cgroup - just write PID to the simulated file
    if [[ -f "$CGROUP_PATH/memory/cage-container/cgroup.procs" ]]; then
        echo "$CONTAINER_PID" > "$CGROUP_PATH/memory/cage-container/cgroup.procs" 2>/dev/null
        log "Added container to simulated memory cgroup"
    fi
    
    # Add to pids cgroup - just write PID to the simulated file
    if [[ -f "$CGROUP_PATH/pids/cage-container/cgroup.procs" ]]; then
        echo "$CONTAINER_PID" > "$CGROUP_PATH/pids/cage-container/cgroup.procs" 2>/dev/null
        log "Added container to simulated PIDs cgroup"
    fi
    
    log "Note: These are simulated cgroups only - no actual resource limits are enforced by the kernel"
}

# Get application command from config.yaml
get_applications_from_config() {
    log "Getting application commands from config"
    
    # Default application command
    APPLICATION_COMMAND="/app/webserver"
    
    # Find the first application in the config
    if grep -q 'applications:' "$CONFIG_FILE"; then
        # Get the command for the first application
        local command=$(grep -A3 'command:' "$CONFIG_FILE" | grep 'command:' | head -1 | cut -d':' -f2 | tr -d ' "' | tr -d '\n' | tr -d '\r')
        
        if [[ -n "$command" ]]; then
            APPLICATION_COMMAND="$command"
            log "Using application command from config: $APPLICATION_COMMAND"
        else
            log "No command found in config, using default: $APPLICATION_COMMAND"
        fi
    else
        warn "No applications section found in config, using default command: $APPLICATION_COMMAND"
    fi
}

# Function to execute a command inside the container
exec_in_container() {
    local command="$1"
    local container_pid=$(get_container_pid)
    
    if [[ -z "$container_pid" ]]; then
        error "Container is not running"
        return 1
    fi
    
    log "Executing command inside container: $command"
    nsenter --target "$container_pid" --mount --uts --ipc --net --pid chroot /mnt/cage-container /bin/sh -c "
        export LD_LIBRARY_PATH=/usr/local/lib:/lib:/usr/lib:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu
        $command
    "
}

# Run the start container function
start_container 