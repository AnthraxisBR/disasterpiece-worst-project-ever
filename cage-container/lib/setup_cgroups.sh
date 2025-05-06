#!/bin/bash

# Setup simulated cgroups for the container

setup_cgroups() {
    log "Setting up simulated cgroup structure"
    
    # Create fake cgroup filesystem
    mkdir -p "$CGROUP_PATH" || {
        warn "Failed to create cgroup path at $CGROUP_PATH"
        return 1
    }
    
    # Mount a tmpfs at the cgroup mount point
    if ! mount | grep -q "$CGROUP_PATH"; then
        mount -t tmpfs none "$CGROUP_PATH" || {
            warn "Failed to mount tmpfs for simulated cgroups"
            return 1
        }
    fi
    
    # Create the basic cgroup hierarchy
    mkdir -p "$CGROUP_PATH/cpu"
    mkdir -p "$CGROUP_PATH/memory"
    mkdir -p "$CGROUP_PATH/pids"
    
    # Just create directories without actual mounting - simulation only
    log "Creating simulated cgroup controllers"
    
    # Create container cgroup directories
    mkdir -p "$CGROUP_PATH/cpu/cage-container"
    mkdir -p "$CGROUP_PATH/memory/cage-container"
    mkdir -p "$CGROUP_PATH/pids/cage-container"
    
    # Create simulated cgroup files
    create_simulated_cgroup_files
    
    # Create cgroup_monitor.sh to simulate monitoring
    create_cgroup_monitor
    
    success "Simulated cgroup structure completed"
}

# Create simulated cgroup files with resource limits
create_simulated_cgroup_files() {
    log "Creating simulated cgroup control files"
    
    # CPU controller
    echo "100000" > "$CGROUP_PATH/cpu/cpu.cfs_period_us"
    echo "$CPU_QUOTA" > "$CGROUP_PATH/cpu/cage-container/cpu.cfs_quota_us"
    echo "0" > "$CGROUP_PATH/cpu/cage-container/cpu.cfs_burst_us"
    echo "0" > "$CGROUP_PATH/cpu/cage-container/cpu.stat"
    touch "$CGROUP_PATH/cpu/cage-container/cgroup.procs"
    
    # Memory controller
    echo "$MEMORY_BYTES" > "$CGROUP_PATH/memory/cage-container/memory.limit_in_bytes"
    echo "0" > "$CGROUP_PATH/memory/cage-container/memory.usage_in_bytes"
    echo "0" > "$CGROUP_PATH/memory/cage-container/memory.stat"
    touch "$CGROUP_PATH/memory/cage-container/cgroup.procs"
    
    # PIDs controller
    echo "50" > "$CGROUP_PATH/pids/cage-container/pids.max"
    echo "0" > "$CGROUP_PATH/pids/cage-container/pids.current"
    touch "$CGROUP_PATH/pids/cage-container/cgroup.procs"
    
    log "Simulated cgroup files created with CPU quota: $CPU_QUOTA, Memory: $MEMORY_BYTES bytes"
}

# Create the cgroup monitor script
create_cgroup_monitor() {
    log "Creating cgroup monitor script for simulation"
    
    local monitor_script="$ROOTFS/app/cgroup_monitor.sh"
    
    cat > "$monitor_script" << 'EOF'
#!/bin/bash

# Simulated Cgroup Monitor for Cage Container
# This script simulates cgroup monitoring inside the container

CGROUP_PATH="/sys/fs/cgroup"
LOG_FILE="/logs/cgroup_monitor.log"
CONFIG_FILE="/app/container_config.json"

# Create log directory if it doesn't exist
mkdir -p /logs

echo "[LOG] Simulated cgroup monitor started" | tee -a "$LOG_FILE"

# Function to simulate cgroup values
simulate_cgroup_values() {
    # Get current memory usage from process
    MEM_USAGE=$(ps -o rss= -p 1 2>/dev/null || echo "0")
    MEM_USAGE=$((MEM_USAGE * 1024)) # convert KB to bytes
    
    # Get number of processes 
    PROC_COUNT=$(ps -A --no-headers | wc -l 2>/dev/null || echo "0")
    
    # Write simulated values to cgroup files
    echo "$MEM_USAGE" > "$CGROUP_PATH/memory/cage-container/memory.usage_in_bytes" 2>/dev/null || true
    echo "$PROC_COUNT" > "$CGROUP_PATH/pids/cage-container/pids.current" 2>/dev/null || true
    
    # Calculate memory percentage (avoid division by zero)
    if [ "$MEMORY_LIMIT" -gt 0 ]; then
        MEM_PCT=$((MEM_USAGE * 100 / MEMORY_LIMIT))
    else
        MEM_PCT=0
    fi
    
    # Log current state
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Memory: $MEM_USAGE bytes ($MEM_PCT%), Processes: $PROC_COUNT" | tee -a "$LOG_FILE"
    
    # Check limits
    if [ $MEM_PCT -gt 80 ]; then
        echo "[WARN] High memory usage: $MEM_PCT%" | tee -a "$LOG_FILE"
    fi
    
    if [ $PROC_COUNT -gt 40 ]; then
        echo "[WARN] High process count: $PROC_COUNT" | tee -a "$LOG_FILE"
    fi
}

# Default resource limits
MEMORY_LIMIT=134217728
PID_LIMIT=50

# Main loop
while true; do
    simulate_cgroup_values
    sleep 10
done
EOF
    
    # Make the script executable
    chmod +x "$monitor_script"
    log "Cgroup monitor script created at $monitor_script"
}

# Run the setup
setup_cgroups 