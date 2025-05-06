#!/bin/bash

# Reset script to completely wipe all container state and PID files
# Use this when the container is in an inconsistent state

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log() { echo -e "${BLUE}[LOG]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }
success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }

log "Starting complete reset of cage-container..."

# Remove PID files and container logs
log "Removing PID files and logs..."
rm -f "$SCRIPT_DIR/logs/container.pid" "$SCRIPT_DIR/logs/socat.pid" "$SCRIPT_DIR/logs/nc.pid" 2>/dev/null || true
rm -f "$SCRIPT_DIR/logs/port_forward.info" 2>/dev/null || true
rm -f "$SCRIPT_DIR/logs/container.log" 2>/dev/null || true

# Try to run cleanup script
if [[ -f "$SCRIPT_DIR/cleanup-all.sh" ]]; then
    log "Running cleanup-all.sh..."
    sudo "$SCRIPT_DIR/cleanup-all.sh" || warn "Cleanup script failed, continuing with reset"
else
    warn "cleanup-all.sh not found, skipping comprehensive cleanup"
fi

# WSL-specific - kill all related processes by name
log "Killing any remaining processes..."
pkill -f cage-container 2>/dev/null || true

# WSL-specific - WSL doesn't always clean up mounts properly
log "Cleaning up mounts (WSL-specific)..."
if [[ -f /proc/mounts ]]; then
    cat /proc/mounts | grep cage | awk '{print $2}' | while read mount_point; do
        log "Trying to unmount $mount_point"
        sudo umount -f "$mount_point" 2>/dev/null || true
    done
fi

# Remove squashfs image
log "Removing squashfs image..."
sudo rm -f /var/cage-container.img 2>/dev/null || true

# Remove any temporary files
log "Removing temporary files..."
if [[ -d /mnt/cage-container ]]; then
    sudo rm -rf /mnt/cage-container/* 2>/dev/null || true
fi

# Clean up any iptables rules (WSL limitations)
if command -v iptables &>/dev/null; then
    log "Cleaning up iptables rules..."
    sudo iptables -t nat -F 2>/dev/null || true
fi

# WSL specific - ensure ports are closed
if command -v netstat &>/dev/null; then
    log "Checking for any occupied ports..."
    ports=$(netstat -tuln | grep 8080 | awk '{print $4}' | cut -d: -f2)
    if [[ -n "$ports" ]]; then
        warn "Port 8080 is still in use. Consider restarting WSL with:"
        warn "   wsl --shutdown"
        warn "   wsl"
    fi
fi

# Make port 8080 available again (may require sudo)
if command -v ss &>/dev/null; then
    pid=$(ss -lptn "sport = :8080" 2>/dev/null | grep -oP '(?<=pid=)(\d+)' | head -1)
    if [[ -n "$pid" ]]; then
        log "Killing process $pid that's using port 8080"
        sudo kill -9 "$pid" 2>/dev/null || true
    fi
fi

# Create logs directory
mkdir -p "$SCRIPT_DIR/logs"

success "Reset completed! You can now start a fresh container."
log "Run with: sudo $SCRIPT_DIR/cage-container.sh -p 8080 /path/to/your/application" 