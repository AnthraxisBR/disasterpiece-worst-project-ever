#!/bin/bash

# Setup port forwarding between host and container

# Default web server port inside container
CONTAINER_PORT=8080

# Setup port forwarding for the container
setup_port_forwarding() {
    local host_port="$1"
    
    if [[ -z "$host_port" ]]; then
        host_port=8080
    fi
    
    log "Setting up port forwarding from host port $host_port to container port $CONTAINER_PORT"
    
    # Ensure IP forwarding is enabled
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || {
        warn "Failed to enable IP forwarding"
    }
    
    # Save port forwarding info for cleanup and status
    echo "$host_port:$CONTAINER_PORT" > "$SCRIPT_DIR/logs/port_forward.info"
    
    # Verify container IP is accessible first
    if ! ping -c 1 -W 1 "$IP_ONLY" &>/dev/null; then
        warn "Container IP $IP_ONLY is not pingable, port forwarding may not work"
        log "Make sure the container network is properly set up"
    fi
    
    # Helper function to check if a tool is available
    tool_available() {
        command -v "$1" &>/dev/null
    }
    
    # Try multiple port forwarding methods - from most reliable to least
    PORT_FORWARDING_SUCCESS=false
    
    # Method 1: socat - most reliable
    if tool_available socat; then
        log "Starting socat for port forwarding (recommended method)"
        
        # Kill any existing socat processes using this port
        pkill -f "socat.*:$host_port" 2>/dev/null || true
        
        # Start socat in the background, with proper redirection
        (socat TCP-LISTEN:$host_port,fork,reuseaddr TCP:$IP_ONLY:$CONTAINER_PORT > "$SCRIPT_DIR/logs/socat.log" 2>&1) &
        SOCAT_PID=$!
        
        # Verify socat started successfully
        sleep 1
        if kill -0 $SOCAT_PID 2>/dev/null; then
            echo "$SOCAT_PID" > "$SCRIPT_DIR/logs/socat.pid"
            success "Port forwarding established using socat (PID: $SOCAT_PID)"
            log "Access the container at http://localhost:$host_port"
            echo "socat" > "$SCRIPT_DIR/logs/port_forward.method"
            PORT_FORWARDING_SUCCESS=true
        else
            warn "Socat started but exited immediately, trying alternative methods"
            log "Check $SCRIPT_DIR/logs/socat.log for errors"
        fi
    else
        log "Socat not found, trying alternative methods"
    fi
    
    # Method 2: iptables (if we didn't succeed with socat)
    if [ "$PORT_FORWARDING_SUCCESS" != "true" ] && tool_available iptables; then
        log "Trying iptables for port forwarding"
        
        # Clean up any existing rules for this port
        iptables -t nat -D PREROUTING -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
        iptables -t nat -D OUTPUT -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
        
        # Set up new rules - capture any errors
        IPTABLES_OUT=$(
            iptables -t nat -A PREROUTING -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>&1 && \
            iptables -t nat -A OUTPUT -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>&1 && \
            iptables -t nat -A POSTROUTING -s "$IP_ONLY" -j MASQUERADE 2>&1
        )
        
        if [ $? -eq 0 ]; then
            success "Port forwarding established using iptables"
            log "Access the container at http://localhost:$host_port"
            echo "iptables" > "$SCRIPT_DIR/logs/port_forward.method"
            PORT_FORWARDING_SUCCESS=true
        else
            warn "Failed to set up iptables port forwarding: $IPTABLES_OUT"
            log "Trying alternative methods"
        fi
    elif [ "$PORT_FORWARDING_SUCCESS" != "true" ]; then
        log "iptables not found or already succeeded with another method"
    fi
    
    # Method 3: netcat (nc) - if we didn't succeed with previous methods
    if [ "$PORT_FORWARDING_SUCCESS" != "true" ] && (tool_available nc || tool_available netcat); then
        NC_CMD="nc"
        if ! tool_available nc && tool_available netcat; then
            NC_CMD="netcat"
        fi
        
        log "Trying $NC_CMD for port forwarding"
        
        # Kill any existing netcat processes
        pkill -f "$NC_CMD.*:$host_port" 2>/dev/null || true
        
        # Determine if we have traditional or BSD netcat
        if $NC_CMD -h 2>&1 | grep -q "\-l"; then
            # Traditional netcat with -l option
            log "Using traditional $NC_CMD"
            if $NC_CMD -h 2>&1 | grep -q "\-c"; then
                # With command option
                NETCAT_CMD="(while true; do $NC_CMD -l \"$host_port\" -c \"$NC_CMD $IP_ONLY $CONTAINER_PORT\"; sleep 0.1; done)"
            else
                # Without command option (BSD style)
                NETCAT_CMD="(while true; do $NC_CMD -l \"$host_port\" | $NC_CMD \"$IP_ONLY\" \"$CONTAINER_PORT\"; sleep 0.1; done)"
            fi
        else
            # Older netcat style
            NETCAT_CMD="(while true; do $NC_CMD -l -p \"$host_port\" | $NC_CMD \"$IP_ONLY\" \"$CONTAINER_PORT\"; sleep 0.1; done)"
        fi
        
        # Start netcat in background
        eval "$NETCAT_CMD > \"$SCRIPT_DIR/logs/nc.log\" 2>&1 &"
        NC_PID=$!
        
        # Verify netcat started successfully
        sleep 1
        if kill -0 $NC_PID 2>/dev/null; then
            echo "$NC_PID" > "$SCRIPT_DIR/logs/nc.pid"
            success "Port forwarding established using $NC_CMD (PID: $NC_PID)"
            log "Access the container at http://localhost:$host_port"
            echo "netcat" > "$SCRIPT_DIR/logs/port_forward.method"
            PORT_FORWARDING_SUCCESS=true
        else
            warn "$NC_CMD started but exited immediately, checking log"
            log "Check $SCRIPT_DIR/logs/nc.log for errors"
        fi
    elif [ "$PORT_FORWARDING_SUCCESS" != "true" ]; then
        log "Netcat not found or already succeeded with another method"
    fi
    
    # Method 4: SSH local port forwarding (if available and didn't succeed with previous methods)
    if [ "$PORT_FORWARDING_SUCCESS" != "true" ] && tool_available ssh; then
        log "Trying SSH local port forwarding"
        
        # SSH local forwarding (assuming localhost is reachable)
        (ssh -f -N -L "$host_port:$IP_ONLY:$CONTAINER_PORT" localhost > "$SCRIPT_DIR/logs/ssh.log" 2>&1) &
        SSH_PID=$!
        
        # Verify SSH started successfully
        sleep 1
        if kill -0 $SSH_PID 2>/dev/null; then
            echo "$SSH_PID" > "$SCRIPT_DIR/logs/ssh.pid"
            success "Port forwarding established using SSH (PID: $SSH_PID)"
            log "Access the container at http://localhost:$host_port"
            echo "ssh" > "$SCRIPT_DIR/logs/port_forward.method"
            PORT_FORWARDING_SUCCESS=true
        else
            warn "SSH port forwarding attempt failed, checking log"
            log "Check $SCRIPT_DIR/logs/ssh.log for errors"
        fi
    elif [ "$PORT_FORWARDING_SUCCESS" != "true" ]; then
        log "SSH not found or already succeeded with another method"
    fi
    
    # Method 5: Pure bash TCP proxy (last resort)
    if [ "$PORT_FORWARDING_SUCCESS" != "true" ]; then
        log "Trying pure bash TCP proxy (last resort)"
        
        # Create a bash script to handle the TCP proxy
        PROXY_SCRIPT="$SCRIPT_DIR/logs/tcp_proxy.sh"
        
        cat > "$PROXY_SCRIPT" << EOF
#!/bin/bash

# Pure bash TCP proxy from host port $host_port to $IP_ONLY:$CONTAINER_PORT
# This is a fallback when no other port forwarding methods are available

echo "Starting pure bash TCP proxy on port $host_port -> $IP_ONLY:$CONTAINER_PORT"

while true; do
    # Try to create a server socket on host port
    exec 3<>/dev/tcp/0.0.0.0/$host_port || { echo "Failed to bind to port $host_port"; sleep 5; continue; }
    
    echo "Accepted connection on port $host_port"
    
    # Try to connect to the target
    exec 4<>/dev/tcp/$IP_ONLY/$CONTAINER_PORT || { echo "Failed to connect to $IP_ONLY:$CONTAINER_PORT"; exec 3>&-; sleep 1; continue; }
    
    # Forward data in both directions (very crude, may not work well for HTTP)
    # This is just a last resort
    cat <&3 >&4 &
    cat <&4 >&3
    
    # Clean up
    exec 3>&-
    exec 4>&-
    
    sleep 0.1
done
EOF
        
        chmod +x "$PROXY_SCRIPT"
        
        # Start the proxy script
        "$PROXY_SCRIPT" > "$SCRIPT_DIR/logs/tcp_proxy.log" 2>&1 &
        PROXY_PID=$!
        
        # Verify proxy started
        sleep 1
        if kill -0 $PROXY_PID 2>/dev/null; then
            echo "$PROXY_PID" > "$SCRIPT_DIR/logs/tcp_proxy.pid"
            log "TCP proxy started with PID $PROXY_PID (this is a last resort and may not work well)"
            log "Access the container at http://localhost:$host_port (if the proxy works)"
            echo "bash_proxy" > "$SCRIPT_DIR/logs/port_forward.method"
            PORT_FORWARDING_SUCCESS=true
        else
            error "Pure bash TCP proxy failed to start"
        fi
    fi
    
    # If we got here and no method worked
    if [ "$PORT_FORWARDING_SUCCESS" != "true" ]; then
        error "Failed to set up port forwarding using any available method"
        log "Available port forwarding tools:"
        log "  - socat: $(command -v socat >/dev/null 2>&1 && echo "Available" || echo "Not available")"
        log "  - iptables: $(command -v iptables >/dev/null 2>&1 && echo "Available" || echo "Not available")"
        log "  - nc/netcat: $(command -v nc >/dev/null 2>&1 || command -v netcat >/dev/null 2>&1 && echo "Available" || echo "Not available")"
        log "  - ssh: $(command -v ssh >/dev/null 2>&1 && echo "Available" || echo "Not available")"
        error "Please install socat for reliable port forwarding: apt-get install socat"
        return 1
    fi
    
    return 0
}

# Cleanup port forwarding
cleanup_port_forwarding() {
    log "Cleaning up port forwarding"
    
    # Get port forwarding info
    if [[ -f "$SCRIPT_DIR/logs/port_forward.info" ]]; then
        local port_info=$(cat "$SCRIPT_DIR/logs/port_forward.info")
        local host_port=$(echo "$port_info" | cut -d':' -f1)
        
        # Clean up based on the method used
        local method="unknown"
        if [[ -f "$SCRIPT_DIR/logs/port_forward.method" ]]; then
            method=$(cat "$SCRIPT_DIR/logs/port_forward.method")
        fi
        
        log "Cleaning up port forwarding (method: $method, port: $host_port)"
        
        # Clean up iptables rules (do this regardless of method)
        if command -v iptables &> /dev/null; then
            iptables -t nat -D PREROUTING -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
            iptables -t nat -D OUTPUT -p tcp --dport "$host_port" -j DNAT --to-destination "$IP_ONLY:$CONTAINER_PORT" 2>/dev/null || true
            iptables -t nat -D POSTROUTING -s "$IP_ONLY" -j MASQUERADE 2>/dev/null || true
        fi
        
        # Kill socat if running
        if [[ -f "$SCRIPT_DIR/logs/socat.pid" ]]; then
            local socat_pid=$(cat "$SCRIPT_DIR/logs/socat.pid")
            log "Killing socat process ($socat_pid)"
            kill "$socat_pid" 2>/dev/null || true
            pkill -f "socat.*:$host_port" 2>/dev/null || true
            rm -f "$SCRIPT_DIR/logs/socat.pid"
        fi
        
        # Kill netcat if running
        if [[ -f "$SCRIPT_DIR/logs/nc.pid" ]]; then
            local nc_pid=$(cat "$SCRIPT_DIR/logs/nc.pid")
            log "Killing netcat process ($nc_pid)"
            kill "$nc_pid" 2>/dev/null || true
            pkill -f "nc.*:$host_port" 2>/dev/null || true
            pkill -f "netcat.*:$host_port" 2>/dev/null || true
            rm -f "$SCRIPT_DIR/logs/nc.pid"
        fi
        
        # Kill SSH if running
        if [[ -f "$SCRIPT_DIR/logs/ssh.pid" ]]; then
            local ssh_pid=$(cat "$SCRIPT_DIR/logs/ssh.pid")
            log "Killing SSH forwarding process ($ssh_pid)"
            kill "$ssh_pid" 2>/dev/null || true
            rm -f "$SCRIPT_DIR/logs/ssh.pid"
        fi
        
        # Kill bash proxy if running
        if [[ -f "$SCRIPT_DIR/logs/tcp_proxy.pid" ]]; then
            local proxy_pid=$(cat "$SCRIPT_DIR/logs/tcp_proxy.pid")
            log "Killing TCP proxy process ($proxy_pid)"
            kill "$proxy_pid" 2>/dev/null || true
            pkill -f "tcp_proxy.sh" 2>/dev/null || true
            rm -f "$SCRIPT_DIR/logs/tcp_proxy.pid"
        fi
        
        # Remove port forwarding info
        rm -f "$SCRIPT_DIR/logs/port_forward.info"
        rm -f "$SCRIPT_DIR/logs/port_forward.method"
        
        # Ensure port is released
        if command -v ss &> /dev/null; then
            pid=$(ss -lptn "sport = :$host_port" 2>/dev/null | grep -oP '(?<=pid=)(\d+)' | head -1)
            if [[ -n "$pid" ]]; then
                warn "Port $host_port is still in use by PID $pid. Attempting to kill it."
                kill -9 "$pid" 2>/dev/null || true
            fi
        fi
    else
        log "No port forwarding info found, skipping cleanup"
    fi
} 