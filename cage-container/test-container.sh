#!/bin/bash

# Test script for cage-container
# This script runs a series of tests to verify that the container works correctly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Color codes for output
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

# Make sure we clean up on exit
trap cleanup EXIT INT TERM

# Clean up function
cleanup() {
    log "Cleaning up test environment..."
    "$SCRIPT_DIR/cage-container.sh" --stop >/dev/null 2>&1 || true
    log "Test cleanup completed"
}

# Run a test and report results
run_test() {
    local test_name="$1"
    local test_cmd="$2"
    local expected_result="$3"
    
    log "Running test: $test_name"
    
    # Run the command and capture output
    local output
    if output=$(eval "$test_cmd" 2>&1); then
        local status=$?
        
        # Check if output contains expected result
        if echo "$output" | grep -q "$expected_result"; then
            success "✓ Test passed: $test_name"
            return 0
        else
            error "✗ Test failed: $test_name"
            error "  Expected output containing: $expected_result"
            error "  Actual output: $output"
            return 1
        fi
    else
        local status=$?
        error "✗ Test failed: $test_name (exit code: $status)"
        error "  Output: $output"
        return 1
    fi
}

# Start test suite
log "Starting cage-container test suite..."

# Ensure we're not already running
if "$SCRIPT_DIR/cage-container.sh" --status >/dev/null 2>&1; then
    error "Container is already running, please stop it before running tests"
    exit 1
fi

# Test 1: Reset container
run_test "Container reset" \
    "$SCRIPT_DIR/cage-container.sh --reset" \
    "Reset completed"

# Test 2: Check status when container is not running
run_test "Status check (no container)" \
    "$SCRIPT_DIR/cage-container.sh --status" \
    "No container is running"

# Test 3: Check process list when container is not running
run_test "Process list (no container)" \
    "$SCRIPT_DIR/cage-container.sh --ps" \
    "No container is running"

# Create a simple test application
mkdir -p /tmp/cage-test-app
cat > /tmp/cage-test-app/test-app.sh << 'EOF'
#!/bin/sh
echo "Test application started"
echo "Container hostname: $(hostname)"
echo "Process ID: $$"
# Keep running to allow status checks
sleep 600
EOF
chmod +x /tmp/cage-test-app/test-app.sh

# Test 4: Start container with test app
log "Starting container with test application..."
"$SCRIPT_DIR/cage-container.sh" -p 8888 /tmp/cage-test-app >/dev/null 2>&1 &
container_pid=$!

# Wait for container to start
sleep 5

# Test 5: Check if container is running
run_test "Container running check" \
    "$SCRIPT_DIR/cage-container.sh --status" \
    "Container is running"

# Test 6: Check process list
run_test "Process list check" \
    "$SCRIPT_DIR/cage-container.sh --ps" \
    "Container processes"

# Test 7: Execute command in container
run_test "Execute command in container" \
    "$SCRIPT_DIR/cage-container.sh --exec 'echo hello from container'" \
    "hello from container"

# Test 8: Stop container
run_test "Stop container" \
    "$SCRIPT_DIR/cage-container.sh --stop" \
    "Container cleanup completed"

# Test 9: Verify container stopped
run_test "Verify container stopped" \
    "$SCRIPT_DIR/cage-container.sh --status" \
    "No container is running"

# Test 10: Port forwarding test
log "Starting container with port forwarding for network test..."
"$SCRIPT_DIR/cage-container.sh" -p 8888 /tmp/cage-test-app >/dev/null 2>&1 &
container_pid=$!

# Wait for container to start
sleep 5

# Try to connect to the port
if command -v curl >/dev/null 2>&1; then
    # Create a simple HTTP server in the container
    "$SCRIPT_DIR/cage-container.sh" --exec 'cd /tmp && echo "HTTP/1.1 200 OK\n\nHello from container" > response.txt && while true; do { echo -e "HTTP/1.1 200 OK\r\n\r\nHello from container"; } | nc -l 8080; done' >/dev/null 2>&1 &
    
    # Wait for server to start
    sleep 2
    
    # Test connection
    run_test "Port forwarding" \
        "curl -s http://localhost:8888 || echo 'Connection failed'" \
        "Hello from container"
else
    warn "curl not found, skipping port forwarding test"
fi

# Stop container from test 10
"$SCRIPT_DIR/cage-container.sh" --stop >/dev/null 2>&1

# Clean up test app
rm -rf /tmp/cage-test-app

log "All tests completed"
exit 0 