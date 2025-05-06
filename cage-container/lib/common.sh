#!/bin/bash

# Common variables and functions for cage-container

# Global variables
ROOTFS=""
NET_NAMESPACE=""
NET_IP=""
NET_INTERFACE=""
VETH_PEER=""
MEMORY_LIMIT=""
CPU_LIMIT=""
CGROUP_PATH=""
CONTAINER_PID_FILE="$SCRIPT_DIR/logs/container.pid"

# Color codes for logs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "${BLUE}[LOG]${NC} $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$SCRIPT_DIR/logs/container.log"
}

# Function to load configuration from YAML file
load_config() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        error "Configuration file not found: $config_file"
        exit 1
    fi
    
    log "Loading configuration from $config_file"
    
    # Read values from config.yaml
    ROOTFS=$(grep '^rootfs:' "$config_file" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    NET_NAMESPACE=$(grep 'namespace:' "$config_file" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    NET_IP=$(grep 'address:' "$config_file" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    NET_INTERFACE=$(grep 'net_interface:' "$config_file" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    VETH_PEER=$(grep 'veth_peer:' "$config_file" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    
    MEMORY_LIMIT=$(grep -A3 'applications:' "$config_file" | grep 'memory:' | awk '{print $2}' | tr -d 'M' | tr -d '"' | tr -d '\n' | tr -d '\r')
    CPU_LIMIT=$(grep -A3 'applications:' "$config_file" | grep 'cpu:' | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    
    # Set defaults if not specified
    if [[ -z "$MEMORY_LIMIT" ]]; then
        MEMORY_LIMIT=1024
    fi
    
    if [[ -z "$CPU_LIMIT" ]]; then
        CPU_LIMIT=1
    fi
    
    # Validate configuration
    if [[ -z "$ROOTFS" ]]; then
        error "Root filesystem path not found in config.yaml"
        exit 1
    fi
    
    if [[ -z "$NET_NAMESPACE" ]]; then
        error "Network namespace not found in config.yaml"
        exit 1
    fi
    
    if [[ -z "$NET_IP" ]]; then
        error "Network IP not found in config.yaml"
        exit 1
    fi
    
    if [[ -z "$NET_INTERFACE" ]]; then
        error "Network interface not found in config.yaml"
        exit 1
    fi
    
    if [[ -z "$VETH_PEER" ]]; then
        error "Veth peer not found in config.yaml"
        exit 1
    fi
    
    # Convert memory limit to bytes
    MEMORY_BYTES=$((MEMORY_LIMIT * 1024 * 1024))
    
    # Convert CPU limit to quota (in microseconds)
    CPU_QUOTA=$((CPU_LIMIT * 100000))
    
    # Set cgroup path
    CGROUP_PATH="$ROOTFS/sys/fs/cgroup"
    
    # Parse IP and subnet mask
    IP_ONLY=$(echo "$NET_IP" | cut -d'/' -f1 | tr -d '\n' | tr -d '\r')
    SUBNET_MASK=$(echo "$NET_IP" | cut -d'/' -f2 | tr -d '\n' | tr -d '\r')
    
    if [[ -z "$IP_ONLY" || -z "$SUBNET_MASK" ]]; then
        error "Invalid network configuration: $NET_IP"
        exit 1
    fi
    
    log "Configuration loaded successfully"
}

# Check if the command exists
check_command() {
    if ! command -v "$1" &> /dev/null; then
        error "Required command not found: $1"
        exit 1
    fi
}

# Check required tools
check_required_tools() {
    check_command ip
    check_command unshare
    check_command mount
    check_command umount
    check_command mksquashfs
    check_command chroot
    check_command nsenter
    
    # Check for cgroup namespace support
    if [ ! -f "/proc/self/ns/cgroup" ]; then
        error "Your system does not support cgroup namespaces!"
        exit 1
    fi
}

# Function to get container PID
get_container_pid() {
    if [[ -f "$CONTAINER_PID_FILE" ]]; then
        cat "$CONTAINER_PID_FILE"
    else
        echo ""
    fi
} 