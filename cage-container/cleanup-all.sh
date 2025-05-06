#!/bin/bash

# Force cleanup of all cage-container resources
# This script will aggressively clean up all resources used by cage-container

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/config.yaml"

# Source common functions if available
if [[ -f "$SCRIPT_DIR/lib/common.sh" ]]; then
    source "$SCRIPT_DIR/lib/common.sh"
else
    # Define minimal logging functions if common.sh is not available
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    NC='\033[0m'
    log() { echo -e "${BLUE}[LOG]${NC} $1"; }
    warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
    error() { echo -e "${RED}[ERROR]${NC} $1"; }
    success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
fi

# First, try a normal cleanup through the main script
log "Attempting normal cleanup via main script..."
"$SCRIPT_DIR/cage-container.sh" --stop 2>/dev/null || true

# Get rootfs from config
if [[ -f "$CONFIG_FILE" ]]; then
    ROOTFS=$(grep '^rootfs:' "$CONFIG_FILE" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    if [[ -z "$ROOTFS" ]]; then
        ROOTFS="/var/cage-container"  # Default
    fi
else
    ROOTFS="/var/cage-container"  # Default
fi

# Get namespace from config
if [[ -f "$CONFIG_FILE" ]]; then
    NET_NAMESPACE=$(grep 'namespace:' "$CONFIG_FILE" | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
    if [[ -z "$NET_NAMESPACE" ]]; then
        NET_NAMESPACE="cage-container"  # Default
    fi
else
    NET_NAMESPACE="cage-container"  # Default
fi

log "Force cleaning all cage-container resources..."

# Find and kill any processes using the rootfs
log "Killing processes using rootfs..."
fuser -k "$ROOTFS" 2>/dev/null || true
fuser -k /mnt/cage-container 2>/dev/null || true

# Find and kill processes with cage-container in their command line
log "Killing cage-container processes..."
ps aux | grep cage-container | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true

# Unmount all mounts related to the container
log "Unmounting all container-related filesystems..."

# Find all mounts related to cage-container and unmount them
mount | grep cage-container | awk '{print $3}' | while read mount_point; do
    log "Unmounting $mount_point"
    umount -f "$mount_point" 2>/dev/null || true
done

# Specifically target common mount points
umount -f /mnt/cage-container 2>/dev/null || true
umount -f "$ROOTFS/proc" 2>/dev/null || true

# Find all cgroup mounts and unmount them
if [[ -d "$ROOTFS/sys/fs/cgroup" ]]; then
    log "Unmounting cgroup filesystem..."
    mount | grep "$ROOTFS/sys/fs/cgroup" | awk '{print $3}' | while read mount_point; do
        umount -f "$mount_point" 2>/dev/null || true
    done
    umount -f "$ROOTFS/sys/fs/cgroup" 2>/dev/null || true
fi

# Release loop devices
log "Releasing loop devices..."
losetup -a | grep cage-container | cut -d: -f1 | xargs -r losetup -d 2>/dev/null || true
losetup -D 2>/dev/null || true  # Detach all loop devices

# Remove network namespace and interfaces
log "Cleaning up network..."
if command -v ip &>/dev/null; then
    if ip netns list 2>/dev/null | grep -q "$NET_NAMESPACE"; then
        ip netns del "$NET_NAMESPACE" 2>/dev/null || true
    fi
    
    # Find and delete veth interfaces
    ip link show | grep veth | cut -d: -f2 | awk '{print $1}' | while read iface; do
        ip link del "$iface" 2>/dev/null || true
    done
fi

# Clean up iptables rules
log "Cleaning up iptables rules..."
if command -v iptables &>/dev/null; then
    # Find and remove NAT rules related to cage-container
    iptables -t nat -S 2>/dev/null | grep -i cage | while read rule; do
        # Convert rule to delete command
        rule=$(echo "$rule" | sed 's/-A/-D/')
        iptables -t nat $rule 2>/dev/null || true
    done
    
    # Other general cleanup of port forwarding
    iptables -t nat -F 2>/dev/null || true
fi

# Remove socat and netcat processes used for port forwarding
log "Killing port forwarding processes..."
ps aux | grep -E '(socat|nc) .*[0-9]+' | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true

# Remove PID files
log "Removing PID files..."
rm -f "$SCRIPT_DIR/logs/container.pid" 2>/dev/null || true
rm -f "$SCRIPT_DIR/logs/socat.pid" 2>/dev/null || true
rm -f "$SCRIPT_DIR/logs/nc.pid" 2>/dev/null || true
rm -f "$SCRIPT_DIR/logs/port_forward.info" 2>/dev/null || true

# Clean up squashfs image
log "Removing SquashFS image..."
rm -f /var/cage-container.img 2>/dev/null || true

success "Cleanup completed. System should now be in a clean state."
log "You can now start a new container with: $SCRIPT_DIR/cage-container.sh [options] PROJECT_PATH" 