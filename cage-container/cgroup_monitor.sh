#!/bin/bash

# Enhanced Cgroup Monitor for Cage Container
# This script monitors and maintains cgroup settings inside the container

CGROUP_PATH="/sys/fs/cgroup"
LOG_FILE="/logs/cgroup_monitor.log"
CONFIG_FILE="/app/container_config.json"

# Color codes for logs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "${BLUE}[LOG]${NC} $1" | tee -a "$LOG_FILE"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$LOG_FILE"
}

# Create log directory if it doesn't exist
mkdir -p /logs

# Default resource limits
CPU_QUOTA=50000        # 50% of CPU (100000 is 100%)
MEMORY_LIMIT=134217728 # 128MB in bytes
PID_LIMIT=50           # Maximum number of processes

log "Cgroup monitor started"

# Create dynamic config file if it doesn't exist
if [ ! -f "$CONFIG_FILE" ]; then
    log "Creating default configuration"
    cat > "$CONFIG_FILE" << EOF
{
    "resources": {
        "cpu": 0.5,
        "memory_mb": 128,
        "pids": 50
    },
    "monitoring": {
        "enabled": true,
        "interval": 5
    }
}
EOF
fi

# Function to read configuration from JSON file
read_config() {
    if [ -f "$CONFIG_FILE" ]; then
        # Extract values from JSON if jq is available
        if command -v jq >/dev/null 2>&1; then
            CPU_VALUE=$(jq -r '.resources.cpu' "$CONFIG_FILE" 2>/dev/null)
            MEMORY_MB=$(jq -r '.resources.memory_mb' "$CONFIG_FILE" 2>/dev/null)
            PID_LIMIT=$(jq -r '.resources.pids' "$CONFIG_FILE" 2>/dev/null)
            MONITORING_ENABLED=$(jq -r '.monitoring.enabled' "$CONFIG_FILE" 2>/dev/null)
            MONITORING_INTERVAL=$(jq -r '.monitoring.interval' "$CONFIG_FILE" 2>/dev/null)
            
            # Convert CPU value to quota
            if [[ "$CPU_VALUE" =~ ^[0-9]*(\.[0-9]+)?$ ]]; then
                CPU_QUOTA=$(echo "$CPU_VALUE * 100000" | bc | cut -d. -f1)
            fi
            
            # Convert memory value to bytes
            if [[ "$MEMORY_MB" =~ ^[0-9]+$ ]]; then
                MEMORY_LIMIT=$((MEMORY_MB * 1024 * 1024))
            fi
        else
            # Fallback to grep/sed if jq is not available
            CPU_VALUE=$(grep -o '"cpu":[^,}]*' "$CONFIG_FILE" | sed 's/"cpu"://')
            MEMORY_MB=$(grep -o '"memory_mb":[^,}]*' "$CONFIG_FILE" | sed 's/"memory_mb"://')
            PID_LIMIT=$(grep -o '"pids":[^,}]*' "$CONFIG_FILE" | sed 's/"pids"://')
            MONITORING_INTERVAL=$(grep -o '"interval":[^,}]*' "$CONFIG_FILE" | sed 's/"interval"://')
            
            # Convert CPU value to quota (simplified)
            CPU_QUOTA=$((${CPU_VALUE/.*} * 100000))
            
            # Convert memory value to bytes (simplified)
            MEMORY_LIMIT=$((MEMORY_MB * 1024 * 1024))
        fi
    fi
    
    # Validate and set defaults if needed
    if [ -z "$CPU_QUOTA" ] || [ "$CPU_QUOTA" -le 0 ]; then
        CPU_QUOTA=50000
    fi
    
    if [ -z "$MEMORY_LIMIT" ] || [ "$MEMORY_LIMIT" -le 0 ]; then
        MEMORY_LIMIT=134217728
    fi
    
    if [ -z "$PID_LIMIT" ] || [ "$PID_LIMIT" -le 0 ]; then
        PID_LIMIT=50
    fi
    
    if [ -z "$MONITORING_INTERVAL" ] || [ "$MONITORING_INTERVAL" -le 0 ]; then
        MONITORING_INTERVAL=5
    fi
}

# Set resource limits in cgroups
set_limits() {
    # CPU limits (in microseconds)
    echo "$CPU_QUOTA" > "$CGROUP_PATH/cpu/cpu.cfs_quota_us" 2>/dev/null || 
        warn "Failed to set CPU quota"
    
    # Memory limits (in bytes)
    echo "$MEMORY_LIMIT" > "$CGROUP_PATH/memory/memory.limit_in_bytes" 2>/dev/null || 
        warn "Failed to set memory limit"
    
    # Process count limits
    echo "$PID_LIMIT" > "$CGROUP_PATH/pids/pids.max" 2>/dev/null || 
        warn "Failed to set PID limit"
    
    log "Resource limits applied: CPU=${CPU_QUOTA}us, Memory=${MEMORY_LIMIT}bytes, PIDs=${PID_LIMIT}"
}

# Monitor resource usage and enforce limits
monitor_usage() {
    # Memory usage
    if [ -f "$CGROUP_PATH/memory/memory.usage_in_bytes" ]; then
        MEM_USAGE=$(cat "$CGROUP_PATH/memory/memory.usage_in_bytes" 2>/dev/null || echo "unknown")
        MEM_LIMIT=$(cat "$CGROUP_PATH/memory/memory.limit_in_bytes" 2>/dev/null || echo "unknown")
        
        if [ "$MEM_USAGE" != "unknown" ] && [ "$MEM_LIMIT" != "unknown" ]; then
            MEM_PERCENT=$((MEM_USAGE * 100 / MEM_LIMIT))
            log "Memory usage: ${MEM_USAGE} bytes (${MEM_PERCENT}% of limit)"
            
            # Alert if memory usage is high
            if [ "$MEM_PERCENT" -gt 90 ]; then
                warn "High memory usage detected: ${MEM_PERCENT}% of limit"
            fi
        fi
    fi
    
    # CPU usage (approximate via CPU stats)
    if [ -f "$CGROUP_PATH/cpu/cpu.stat" ]; then
        CPU_USAGE=$(grep 'usage_usec' "$CGROUP_PATH/cpu/cpu.stat" 2>/dev/null | awk '{print $2}')
        if [ -n "$CPU_USAGE" ]; then
            log "CPU usage: ${CPU_USAGE} microseconds"
        fi
    fi
    
    # Process count
    if [ -f "$CGROUP_PATH/pids/pids.current" ]; then
        PID_CURRENT=$(cat "$CGROUP_PATH/pids/pids.current" 2>/dev/null || echo "unknown")
        PID_MAX=$(cat "$CGROUP_PATH/pids/pids.max" 2>/dev/null || echo "unknown")
        
        if [ "$PID_CURRENT" != "unknown" ]; then
            log "Process count: ${PID_CURRENT} (limit: ${PID_MAX})"
            
            # Alert if process count is high
            if [ "$PID_CURRENT" != "unknown" ] && [ "$PID_MAX" != "unknown" ] && [ "$PID_MAX" != "max" ]; then
                PID_PERCENT=$((PID_CURRENT * 100 / PID_MAX))
                if [ "$PID_PERCENT" -gt 80 ]; then
                    warn "High process count detected: ${PID_CURRENT} (${PID_PERCENT}% of limit)"
                fi
            fi
        fi
    fi
}

# Main monitoring loop
log "Starting monitoring loop with interval: ${MONITORING_INTERVAL}s"

while true; do
    # Read current configuration
    read_config
    
    # Apply limits
    set_limits
    
    # Monitor resources
    if [ "$MONITORING_ENABLED" != "false" ]; then
        monitor_usage
    fi
    
    # Wait for next check
    sleep "$MONITORING_INTERVAL"
done
