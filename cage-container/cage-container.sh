#!/bin/bash

# Cage Container: A simple container system using Linux namespaces
# This script coordinates the setup and management of containers

set -e

# Debug flag (set to true for more verbose output)
DEBUG=true

# Get the directory this script is in
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Default config file
CONFIG_FILE="$SCRIPT_DIR/config.yaml"

# Default configuration values
export ROOTFS="/var/cage-container"
export NET_NAMESPACE="cage-container"
export NET_ADDRESS="10.100.0.2/16"
export NET_INTERFACE="veth0"
export VETH_PEER="veth1"
export CGROUP_PATH="/sys/fs/cage-container"
export CGROUP_VERSION="v1"
export CPU_LIMIT=1
export CPU_QUOTA=100000
export MEMORY_LIMIT=1024
export MEMORY_BYTES=$((MEMORY_LIMIT * 1024 * 1024))
export CONTAINER_PID_FILE="/var/run/cage-container.pid"
export PORT_FORWARD_PID_FILE="$SCRIPT_DIR/logs/port_forward.pid"
export HOST_PORT=""
export PROJECT_PATH=""

# Define actions
ACTION="start"
EXEC_COMMAND=""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "[LOG] $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

success() {
    echo -e "[${GREEN}SUCCESS${NC}] $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

warn() {
    echo -e "[${YELLOW}WARN${NC}] $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

error() {
    echo -e "[${RED}ERROR${NC}] $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

debug() {
    if [[ "$DEBUG" == "true" ]]; then
        echo -e "[DEBUG] $1" | tee -a "$SCRIPT_DIR/logs/container.log"
    fi
}

# Load configuration from config.yaml file
load_config() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        error "Configuration file not found: $config_file"
        exit 1
    fi
    
    log "Loading configuration from $config_file"
    
    # Parse the YAML file with grep/awk (limited but functional for simple YAML)
    export ROOTFS=$(grep '^rootfs:' "$config_file" 2>/dev/null | awk '{print $2}' | tr -d '"' | tr -d '\r')
    
    # Network configuration
    local net_namespace=$(grep -A5 'network:' "$config_file" | grep 'namespace:' | awk '{print $2}' | tr -d '"' | tr -d '\r')
    local net_address=$(grep -A5 'network:' "$config_file" | grep 'address:' | awk '{print $2}' | tr -d '"' | tr -d '\r')
    local net_interface=$(grep -A5 'network:' "$config_file" | grep 'net_interface:' | awk '{print $2}' | tr -d '"' | tr -d '\r')
    local veth_peer=$(grep -A5 'network:' "$config_file" | grep 'veth_peer:' | awk '{print $2}' | tr -d '"' | tr -d '\r')
    
    # Resource limits
    local memory_limit=$(grep -A3 'memory:' "$config_file" | grep 'memory:' | head -1 | awk '{print $2}' | tr -d 'M"' | tr -d '\r')
    local cpu_limit=$(grep -A3 'cpu:' "$config_file" | grep 'cpu:' | head -1 | awk '{print $2}' | tr -d '"' | tr -d '\r')
    
    # Set defaults if values not found
    [[ -n "$ROOTFS" ]] || ROOTFS="/var/cage-container"
    [[ -n "$net_namespace" ]] && export NET_NAMESPACE="$net_namespace"
    [[ -n "$net_address" ]] && export NET_ADDRESS="$net_address"
    [[ -n "$net_interface" ]] && export NET_INTERFACE="$net_interface"
    [[ -n "$veth_peer" ]] && export VETH_PEER="$veth_peer"
    
    # Process memory limit (convert M to bytes if needed)
    if [[ -n "$memory_limit" ]]; then
        export MEMORY_LIMIT="$memory_limit"
        export MEMORY_BYTES=$((MEMORY_LIMIT * 1024 * 1024))
    fi
    
    # Process CPU limit (convert to cgroup quota)
    # Handle decimal values using bc for floating point arithmetic
    if [[ -n "$cpu_limit" ]]; then
        export CPU_LIMIT="$cpu_limit"
        
        # Check if bc is available
        if command -v bc &>/dev/null; then
            # Use bc for floating point arithmetic
            export CPU_QUOTA=$(echo "$CPU_LIMIT * 100000" | bc | cut -d. -f1)
        else
            # Fallback for systems without bc: use integer part only
            # Extract the integer part
            local cpu_int=${cpu_limit%.*}
            # If empty (pure decimal like .5), use 0
            [[ -z "$cpu_int" ]] && cpu_int=0
            export CPU_QUOTA=$((cpu_int * 100000))
            
            # For partial CPU, use at least 10% (10000)
            [[ "$CPU_QUOTA" -eq 0 ]] && export CPU_QUOTA=10000
        fi
    fi
    
    # Set the cgroup path inside the rootfs
    export CGROUP_PATH="$ROOTFS/sys/fs/cgroup"
    
    log "Configuration loaded successfully"
    
    # Debug output
    debug "ROOTFS: $ROOTFS"
    debug "NET_NAMESPACE: $NET_NAMESPACE"
    debug "NET_ADDRESS: $NET_ADDRESS"
    debug "CPU_LIMIT: $CPU_LIMIT"
    debug "CPU_QUOTA: $CPU_QUOTA"
    debug "MEMORY_LIMIT: $MEMORY_LIMIT"
    debug "MEMORY_BYTES: $MEMORY_BYTES"
    debug "CGROUP_PATH: $CGROUP_PATH"
}

# Print usage information
print_usage() {
    echo "Usage: $0 [OPTIONS] PROJECT_PATH"
    echo "Where PROJECT_PATH is the path to your application code."
    echo ""
    echo "Options:"
    echo "  -h, --help             Show this help message"
    echo "  --stop                 Stop the running container"
    echo "  --status               Check container status"
    echo "  --ps                   List processes in the container"
    echo "  --reset                Reset the container system (cleanup everything)"
    echo "  --exec COMMAND         Execute a command inside the container"
    echo "  -p, --port PORT        Forward container port 8080 to host port PORT"
    exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            print_usage
            ;;
        --stop)
            ACTION="stop"
            shift
            ;;
        --status)
            ACTION="status"
            shift
            ;;
        --ps)
            ACTION="ps"
            shift
            ;;
        --reset)
            ACTION="reset"
            shift
            ;;
        --exec)
            ACTION="exec"
            EXEC_COMMAND="$2"
            shift 2
            ;;
        -p|--port)
            HOST_PORT="$2"
            shift 2
            ;;
        *)
            if [[ $# -eq 1 && ! "$1" == -* ]]; then
                PROJECT_PATH="$1"
                shift
            else
                echo "[ERROR] Unknown option: $1"
                print_usage
            fi
            ;;
    esac
done

# Create log directory
mkdir -p "$SCRIPT_DIR/logs"

# Load config (needed for any action)
load_config "$CONFIG_FILE"

# Handle SIGINT and SIGTERM for the main script
handle_signals() {
    echo ""
    log "Received signal to terminate"
    source "$SCRIPT_DIR/lib/cleanup.sh"
    cleanup_container
    exit 0
}

# Process actions
case "$ACTION" in
    start)
        # Validate required parameters
        if [[ -z "$PROJECT_PATH" ]]; then
            error "Project path is required for starting a container"
            print_usage
        fi
        
        # Check if a container is already running
        if [[ -f "$CONTAINER_PID_FILE" ]]; then
            container_pid=$(cat "$CONTAINER_PID_FILE")
            if kill -0 "$container_pid" 2>/dev/null; then
                error "Container is already running with PID: $container_pid"
                log "Stop it first with: $0 --stop"
                exit 1
            else
                # Remove stale PID file
                log "Removing stale PID file"
                rm -f "$CONTAINER_PID_FILE"
            fi
        fi
        
        # Run setup phases
        log "Setting up cage-container..."
        
        # Save host port to config if specified
        if [[ -n "$HOST_PORT" ]]; then
            export HOST_PORT
        fi
        
        # Verify project path exists
        if [[ ! -d "$PROJECT_PATH" ]]; then
            error "Project path '$PROJECT_PATH' does not exist or is not a directory"
            exit 1
        fi
        
        # Save project path for later use
        echo "$PROJECT_PATH" > "$SCRIPT_DIR/logs/project_path.info"
        
        # Setup container components
        source "$SCRIPT_DIR/lib/setup_rootfs.sh"
        source "$SCRIPT_DIR/lib/setup_network.sh"
        source "$SCRIPT_DIR/lib/setup_cgroups.sh"
        
        # Start container
        debug "About to start the container by sourcing start_container.sh"
        debug "Current directory: $(pwd)"
        debug "start_container.sh path: $SCRIPT_DIR/lib/start_container.sh"
        
        if [[ -f "$SCRIPT_DIR/lib/start_container.sh" ]]; then
            debug "start_container.sh file exists"
        else
            error "start_container.sh file not found!"
            exit 1
        fi
        
        source "$SCRIPT_DIR/lib/start_container.sh"
        debug "After sourcing start_container.sh"
        
        # This is where the actual start_container function is called
        debug "Calling start_container function"
        start_container
        debug "After calling start_container function"
        
        # Setup port forwarding if host port is specified
        if [[ -n "$HOST_PORT" ]]; then
            source "$SCRIPT_DIR/lib/setup_port_forward.sh"
            setup_port_forwarding "$HOST_PORT"
        fi
        
        # Setup signal handlers for graceful shutdown
        trap 'handle_signals' SIGINT SIGTERM
        
        log "cage-container is running..."
        log "You can stop the container with: $0 --stop"
        log "You can check container status with: $0 --status"
        log "You can list container processes with: $0 --ps"
        log "Container logs available at: $SCRIPT_DIR/logs/"
        
        # Follow container logs
        tail -f "$SCRIPT_DIR/logs/container.log"
        ;;
    
    stop)
        # Just perform cleanup, no need for project path
        source "$SCRIPT_DIR/lib/cleanup.sh"
        cleanup_container
        ;;
    
    status)
        # No need for project path to check status
        source "$SCRIPT_DIR/lib/container_status.sh"
        get_container_status
        ;;
    
    ps)
        # No need for project path to list processes
        source "$SCRIPT_DIR/lib/container_status.sh"
        list_container_processes
        ;;
    
    exec)
        if [[ -z "$EXEC_COMMAND" ]]; then
            error "Command is required for --exec"
            print_usage
        fi
        
        # Check if container is running
        if [[ ! -f "$CONTAINER_PID_FILE" ]]; then
            error "No container is running (no PID file found)"
            exit 1
        fi
        
        source "$SCRIPT_DIR/lib/start_container.sh"
        exec_in_container "$EXEC_COMMAND"
        ;;
    
    reset)
        # Force reset everything
        log "Performing complete reset of cage-container..."
        
        # Source cleanup first to do a regular cleanup
        source "$SCRIPT_DIR/lib/cleanup.sh"
        cleanup_container
        
        # Then run the reset script
        if [[ -f "$SCRIPT_DIR/reset.sh" ]]; then
            log "Running comprehensive reset script..."
            bash "$SCRIPT_DIR/reset.sh"
        else
            # If reset.sh not found, do manual reset steps
            log "Reset script not found, performing manual reset..."
            
            # Kill any remaining processes
            pkill -f cage-container 2>/dev/null || true
            
            # Clean up mounts
            if mount | grep -q "cage-container"; then
                log "Cleaning up mounts..."
                mount | grep cage-container | awk '{print $3}' | xargs -r umount -f 2>/dev/null || true
            fi
            
            # Remove squashfs image
            log "Removing squashfs image..."
            rm -f /var/cage-container.img 2>/dev/null || true
            
            # Clean up network namespaces
            if ip netns list 2>/dev/null | grep -q "$NET_NAMESPACE"; then
                log "Removing network namespace..."
                ip netns del "$NET_NAMESPACE" 2>/dev/null || true
            fi
            
            # Clean up all logs and pid files
            log "Removing logs and PID files..."
            rm -f "$SCRIPT_DIR/logs/"*.pid "$SCRIPT_DIR/logs/"*.info 2>/dev/null || true
            
            success "Reset completed. You can now start a fresh container."
        fi
        ;;
    
    *)
        error "Unknown action: $ACTION"
        print_usage
        ;;
esac

exit 0 