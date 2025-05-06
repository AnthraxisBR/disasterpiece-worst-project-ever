#!/bin/bash

# Setup network for the container

setup_network() {
    log "Setting up network isolation with namespace: $NET_NAMESPACE"
    
    # Clean up existing network namespace and interfaces
    if ip netns list | grep -q "$NET_NAMESPACE"; then
        log "Removing existing network namespace"
        ip netns del "$NET_NAMESPACE" || warn "Failed to delete network namespace"
    fi
    
    if ip link show "$NET_INTERFACE" &> /dev/null; then
        log "Removing existing veth interface"
        ip link del "$NET_INTERFACE" || warn "Failed to delete veth interface"
    fi
    
    # Create new network namespace
    log "Creating network namespace: $NET_NAMESPACE"
    ip netns add "$NET_NAMESPACE" || {
        error "Failed to create network namespace"
        exit 1
    }
    
    # Create veth pair
    log "Creating veth pair: $NET_INTERFACE <-> $VETH_PEER"
    ip link add "$NET_INTERFACE" type veth peer name "$VETH_PEER" || {
        error "Failed to create veth pair"
        exit 1
    }
    
    # Move peer to namespace
    log "Moving $VETH_PEER to namespace $NET_NAMESPACE"
    ip link set "$VETH_PEER" netns "$NET_NAMESPACE" || {
        error "Failed to move veth peer to namespace"
        exit 1
    }
    
    # Configure IP addresses
    log "Configuring IP addresses"
    ip addr add "$IP_ONLY/$SUBNET_MASK" dev "$NET_INTERFACE" || {
        warn "Failed to assign IP to $NET_INTERFACE"
    }
    
    # Bring up the host side interface
    ip link set "$NET_INTERFACE" up || {
        warn "Failed to bring up $NET_INTERFACE"
    }
    
    # Configure the container side
    ip netns exec "$NET_NAMESPACE" ip addr add "$IP_ONLY/$SUBNET_MASK" dev "$VETH_PEER" || {
        warn "Failed to assign IP inside namespace"
    }
    
    ip netns exec "$NET_NAMESPACE" ip link set "$VETH_PEER" up || {
        warn "Failed to bring up $VETH_PEER inside namespace"
    }
    
    # Configure loopback inside namespace
    log "Configuring loopback interface inside namespace"
    ip netns exec "$NET_NAMESPACE" ip link set lo up || {
        warn "Failed to bring up loopback inside namespace"
    }
    
    # Setup DNS inside the container
    setup_dns
    
    # Enable IP forwarding for container connectivity
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || {
        warn "Failed to enable IP forwarding"
    }
    
    # Setup NAT for outbound connectivity (optional)
    setup_nat
    
    success "Network setup completed"
}

# Setup DNS inside the container
setup_dns() {
    log "Setting up DNS configuration"
    
    # Create /etc/resolv.conf inside the container
    mkdir -p "$ROOTFS/etc"
    
    # Use host's DNS or specify custom DNS servers
    if [[ -f "/etc/resolv.conf" ]]; then
        cp "/etc/resolv.conf" "$ROOTFS/etc/resolv.conf" || {
            warn "Failed to copy resolv.conf"
        }
    else
        # Fallback to Google DNS
        echo "nameserver 8.8.8.8" > "$ROOTFS/etc/resolv.conf"
        echo "nameserver 8.8.4.4" >> "$ROOTFS/etc/resolv.conf"
    fi
}

# Setup NAT for outbound connectivity (optional)
setup_nat() {
    log "Setting up NAT for outbound connectivity"
    
    # Check if iptables is available
    if ! command -v iptables &> /dev/null; then
        warn "iptables not found, skipping NAT setup"
        return
    fi
    
    # Add NAT rule for outbound traffic
    iptables -t nat -A POSTROUTING -s "$IP_ONLY/$SUBNET_MASK" -j MASQUERADE 2>/dev/null || {
        warn "Failed to setup NAT rules"
    }
    
    # Allow forwarding between interfaces
    iptables -A FORWARD -i "$NET_INTERFACE" -o eth0 -j ACCEPT 2>/dev/null || true
    iptables -A FORWARD -i eth0 -o "$NET_INTERFACE" -j ACCEPT 2>/dev/null || true
}

# Run the setup
setup_network 