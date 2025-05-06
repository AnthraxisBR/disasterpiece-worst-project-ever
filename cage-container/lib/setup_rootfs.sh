#!/bin/bash

# Setup root filesystem for the container

setup_rootfs() {
    log "Setting up root filesystem at $ROOTFS"
    
    # Ensure any existing processes using the rootfs are killed
    if [[ -d "$ROOTFS" ]]; then
        warn "Cleaning up existing root filesystem"
        fuser -k "$ROOTFS" 2>/dev/null || true
        pkill -9 -f "$ROOTFS" 2>/dev/null || true
        
        # Give processes time to terminate
        sleep 1
    fi
    
    # Check for existing mounts and clean up
    if mount | grep -q "/mnt/cage-container"; then
        warn "Existing mounts found for /mnt/cage-container, attempting to unmount"
        umount -f /mnt/cage-container 2>/dev/null || {
            error "Failed to unmount existing /mnt/cage-container"
            log "Try running the cleanup-all.sh script first"
            exit 1
        }
    fi
    
    # Check for active loop devices and clean up
    if losetup -a | grep -q "cage-container.img"; then
        warn "Existing loop device found for cage-container.img, attempting to detach"
        losetup -a | grep cage-container | cut -d: -f1 | xargs -r losetup -d 2>/dev/null || {
            warn "Failed to detach loop device, trying forced detach of all loops"
            losetup -D 2>/dev/null || true
        }
    fi
    
    # Create directory structure
    mkdir -p "$ROOTFS/bin" "$ROOTFS/etc" "$ROOTFS/tmp" "$ROOTFS/proc" "$ROOTFS/sys" 
    mkdir -p "$ROOTFS/dev" "$ROOTFS/run" "$ROOTFS/logs" "$ROOTFS/usr/bin" "$ROOTFS/app"
    mkdir -p "$ROOTFS/lib/x86_64-linux-gnu" "$ROOTFS/lib64" "$ROOTFS/usr/lib/x86_64-linux-gnu"
    mkdir -p "$ROOTFS/sbin" "$ROOTFS/usr/sbin"
    
    # Copy more essential binaries
    log "Copying essential binaries to the container"
    
    # Basic shell and utilities
    cp /bin/sh "$ROOTFS/bin/" 2>/dev/null || warn "Failed to copy sh"
    cp /bin/bash "$ROOTFS/bin/" 2>/dev/null || warn "Failed to copy bash"
    cp /usr/bin/env "$ROOTFS/usr/bin/" 2>/dev/null || warn "Failed to copy env"
    
    # Essential commands often needed in containers
    ESSENTIAL_BINS=(
        hostname ls cat chmod chown mkdir rm touch
        grep sed awk ps kill
    )
    
    # Network tools for port forwarding
    NETWORK_TOOLS=(
        socat nc netcat iptables ssh
        ip ifconfig ss netstat
    )
    
    # Copy essential tools
    for bin in "${ESSENTIAL_BINS[@]}" "${NETWORK_TOOLS[@]}"; do
        # Try to find the binary in common locations
        for path in /bin /usr/bin /sbin /usr/sbin; do
            if [[ -f "$path/$bin" ]]; then
                # Ensure the target directory exists
                target_dir=$(dirname "$ROOTFS$path/$bin")
                mkdir -p "$target_dir"
                
                # Copy the binary
                cp "$path/$bin" "$ROOTFS$path/$bin" 2>/dev/null && {
                    log "Copied $bin to $ROOTFS$path/$bin"
                    
                    # Check if we need to copy libraries for this binary
                    if command -v ldd &>/dev/null; then
                        log "Copying libraries for $bin"
                        ldd "$path/$bin" 2>/dev/null | grep "=>" | awk '{print $3}' | while read -r lib; do
                            if [[ -f "$lib" && ! -f "$ROOTFS$lib" ]]; then
                                # Create directory structure for library
                                lib_dir=$(dirname "$lib")
                                mkdir -p "$ROOTFS$lib_dir"
                                
                                # Copy the library
                                cp "$lib" "$ROOTFS$lib" 2>/dev/null || warn "Failed to copy $lib for $bin"
                            fi
                        done
                    fi
                    
                    break  # Stop looking if successfully copied
                } || warn "Failed to copy $bin"
            fi
        done
    done
    
    # Special case for socat and iptables - they're often in different locations
    if [ ! -f "$ROOTFS/usr/bin/socat" ] && [ ! -f "$ROOTFS/bin/socat" ]; then
        # Try additional locations
        for socat_path in $(which socat 2>/dev/null) /usr/local/bin/socat; do
            if [[ -f "$socat_path" ]]; then
                mkdir -p "$ROOTFS/usr/local/bin"
                cp "$socat_path" "$ROOTFS/usr/local/bin/" 2>/dev/null && {
                    log "Copied socat to $ROOTFS/usr/local/bin/"
                    chmod +x "$ROOTFS/usr/local/bin/socat"
                    break
                }
            fi
        done
    fi
    
    if [ ! -f "$ROOTFS/sbin/iptables" ] && [ ! -f "$ROOTFS/usr/sbin/iptables" ]; then
        # Try additional locations for iptables
        for iptables_path in $(which iptables 2>/dev/null) /usr/local/sbin/iptables; do
            if [[ -f "$iptables_path" ]]; then
                mkdir -p "$ROOTFS/usr/local/sbin"
                cp "$iptables_path" "$ROOTFS/usr/local/sbin/" 2>/dev/null && {
                    log "Copied iptables to $ROOTFS/usr/local/sbin/"
                    chmod +x "$ROOTFS/usr/local/sbin/iptables"
                    break
                }
            fi
        done
    fi
    
    # Copy application from project path
    log "Copying application from $PROJECT_PATH to $ROOTFS/app"
    if [[ ! -d "$PROJECT_PATH" ]]; then
        error "Project path '$PROJECT_PATH' does not exist or is not a directory"
        exit 1
    fi
    
    cp -r "$PROJECT_PATH/"* "$ROOTFS/app/" 2>/dev/null || {
        error "Failed to copy application from $PROJECT_PATH"
        exit 1
    }
    
    # Make run.sh executable
    if [[ -f "$ROOTFS/app/run.sh" ]]; then
        chmod +x "$ROOTFS/app/run.sh"
        log "Made run.sh executable"
    else
        warn "run.sh not found in application directory"
    fi
    
    # Copy webserver if it exists - check multiple possible locations
    WEBSERVER_FOUND=false
    
    # Check in the project directory
    if [[ -f "$PROJECT_PATH/webserver" ]]; then
        log "Found webserver binary in the project directory"
        cp "$PROJECT_PATH/webserver" "$ROOTFS/app/webserver"
        chmod +x "$ROOTFS/app/webserver"
        WEBSERVER_FOUND=true
    # Check in app directory (might have been copied with cp -r above)
    elif [[ -f "$ROOTFS/app/webserver" ]]; then
        log "Webserver binary found in $ROOTFS/app"
        chmod +x "$ROOTFS/app/webserver"
        WEBSERVER_FOUND=true
    # Check in the webserver subdirectory
    elif [[ -f "$PROJECT_PATH/webserver/webserver" ]]; then
        log "Found webserver binary in the webserver subdirectory"
        mkdir -p "$ROOTFS/app/webserver"
        cp "$PROJECT_PATH/webserver/webserver" "$ROOTFS/app/webserver/webserver"
        chmod +x "$ROOTFS/app/webserver/webserver"
        WEBSERVER_FOUND=true
    # Check in the webserver directory in the workspace
    elif [[ -f "webserver/webserver" ]]; then
        log "Found webserver binary in the workspace webserver directory"
        cp "webserver/webserver" "$ROOTFS/app/webserver"
        chmod +x "$ROOTFS/app/webserver"
        WEBSERVER_FOUND=true
    else
        warn "Webserver binary not found. Checked locations:"
        warn "  - $PROJECT_PATH/webserver"
        warn "  - $PROJECT_PATH/webserver/webserver"
        warn "  - webserver/webserver"
    fi
    
    # Create a simple webserver script as fallback if no binary found or binary is broken
    if [ "$WEBSERVER_FOUND" != "true" ] || ! file "$ROOTFS/app/webserver" | grep -q "executable"; then
        log "Creating a fallback webserver script"
        cat > "$ROOTFS/app/webserver" << 'EOF'
#!/bin/bash

echo "Starting fallback webserver on port 8080..."
echo "This is a fallback script because the actual webserver binary doesn't work."

mkdir -p /logs
echo "Webserver started at $(date)" > /logs/webserver.log

# Simple web server using either netcat or bash
if command -v nc >/dev/null 2>&1; then
    while true; do
        echo -e "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html><body><h1>Cage Container Fake Server</h1><p>The actual webserver binary was not found or couldn't run.</p><p>This is a simulated server running at $(date).</p></body></html>" | nc -l -p 8080
        sleep 0.1
    done
else
    # Pure bash web server
    echo "Netcat not found, using pure bash"
    
    # Create a fifo
    FIFO=/tmp/webfifo
    rm -f $FIFO
    mkfifo $FIFO || { echo "Failed to create FIFO"; exit 1; }
    
    while true; do
        echo "Waiting for connection..."
        cat $FIFO | { 
            read line
            echo "Received: $line"
            
            # Read all headers
            while read line && [ "$line" != $'\r' ]; do
                echo "Header: $line"
            done
            
            # Send response
            echo -e "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html><body><h1>Cage Container Fake Server</h1><p>The actual webserver binary was not found or couldn't run.</p><p>This is a simulated server running at $(date).</p></body></html>"
        } | { 
            # Output goes back to the client
            cat > $FIFO; 
        } &
        
        # Wait a moment before accepting the next connection
        sleep 1
    done
fi
EOF
        chmod +x "$ROOTFS/app/webserver"
        WEBSERVER_FOUND=true
    fi
    
    # Create required device nodes properly
    log "Creating device nodes"
    
    # Remove any existing device nodes first
    rm -f "$ROOTFS/dev/null" "$ROOTFS/dev/zero" "$ROOTFS/dev/random" "$ROOTFS/dev/urandom"
    
    # Create the device nodes with proper permissions
    mknod -m 666 "$ROOTFS/dev/null" c 1 3 || warn "Failed to create /dev/null"
    mknod -m 666 "$ROOTFS/dev/zero" c 1 5 || warn "Failed to create /dev/zero"
    mknod -m 444 "$ROOTFS/dev/random" c 1 8 || warn "Failed to create /dev/random"
    mknod -m 444 "$ROOTFS/dev/urandom" c 1 9 || warn "Failed to create /dev/urandom"
    
    # Verify device nodes were created correctly
    if [[ ! -c "$ROOTFS/dev/null" ]]; then
        warn "Device node /dev/null not created properly, attempting alternate method"
        # Try alternative approach
        mkdir -p "$ROOTFS/dev"
        if [[ -c "/dev/null" ]]; then
            cp -a /dev/null "$ROOTFS/dev/"
            cp -a /dev/zero "$ROOTFS/dev/"
            cp -a /dev/random "$ROOTFS/dev/"
            cp -a /dev/urandom "$ROOTFS/dev/"
        fi
    fi
    
    # Create a minimal /etc/passwd file for the container
    log "Creating /etc/passwd file"
    cat > "$ROOTFS/etc/passwd" << EOF
root:x:0:0:root:/root:/bin/bash
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
EOF
    
    # Create a minimal /etc/group file
    log "Creating /etc/group file"
    cat > "$ROOTFS/etc/group" << EOF
root:x:0:
nobody:x:65534:
EOF
    
    # Create a minimal hosts file
    log "Creating /etc/hosts file"
    cat > "$ROOTFS/etc/hosts" << EOF
127.0.0.1 localhost
127.0.1.1 cage-container
EOF
    
    # Set the hostname
    echo "cage-container" > "$ROOTFS/etc/hostname"
    
    # Mount proc
    log "Mounting procfs"
    mount -t proc proc "$ROOTFS/proc" 2>/dev/null || {
        warn "Failed to mount procfs, attempting to unmount first"
        umount -f "$ROOTFS/proc" 2>/dev/null || true
        mount -t proc proc "$ROOTFS/proc" || {
            error "Failed to mount procfs"
            exit 1
        }
    }
    
    # Setup shared libraries
    setup_libraries
    
    # Remove old squashfs image if it exists
    if [[ -f "/var/cage-container.img" ]]; then
        log "Removing old SquashFS image"
        rm -f /var/cage-container.img 2>/dev/null || {
            warn "Failed to remove old SquashFS image, it may be in use"
            log "Trying to force delete..."
            fuser -k /var/cage-container.img 2>/dev/null || true
            sleep 1
            rm -f /var/cage-container.img 2>/dev/null || warn "Still failed to remove image"
        }
    fi
    
    # Create squashfs image for read-only filesystem
    log "Creating SquashFS image"
    mksquashfs "$ROOTFS" /var/cage-container.img -comp xz -e proc sys dev run tmp 2>/dev/null || {
        error "Failed to create SquashFS image"
        log "Make sure mksquashfs is installed and you have proper permissions"
        exit 1
    }
    
    # Mount the squashfs image
    log "Mounting SquashFS image"
    mkdir -p /mnt/cage-container
    
    # Ensure mount point is clean
    if mount | grep -q "/mnt/cage-container"; then
        log "Mount point is already in use, attempting to unmount"
        umount -f /mnt/cage-container 2>/dev/null
        sleep 1
    fi
    
    # Mount the new image with retries
    max_retries=3
    retry_count=0
    mounted=false
    
    while [ $retry_count -lt $max_retries ] && [ "$mounted" != "true" ]; do
        mount -o loop /var/cage-container.img /mnt/cage-container 2>/dev/null
        if [ $? -eq 0 ]; then
            mounted=true
            break
        else
            retry_count=$((retry_count + 1))
            warn "Mount attempt $retry_count failed, retrying..."
            
            # Cleanup for next attempt
            losetup -D 2>/dev/null || true
            sleep 2
        fi
    done
    
    if [ "$mounted" != "true" ]; then
        error "Failed to mount SquashFS image after $max_retries attempts"
        log "Try running the cleanup-all.sh script first"
        exit 1
    fi
    
    success "Root filesystem setup completed"
}

# Setup required shared libraries
setup_libraries() {
    log "Setting up shared libraries"
    
    # Copy required libraries (adjust these based on your environment)
    cp /lib/x86_64-linux-gnu/libtinfo.so.6 "$ROOTFS/lib/x86_64-linux-gnu/" 2>/dev/null || warn "Failed to copy libtinfo.so.6"
    cp /lib/x86_64-linux-gnu/libc.so.6 "$ROOTFS/lib/x86_64-linux-gnu/" 2>/dev/null || warn "Failed to copy libc.so.6"
    cp /lib64/ld-linux-x86-64.so.2 "$ROOTFS/lib64/" 2>/dev/null || warn "Failed to copy ld-linux-x86-64.so.2"
    
    # Fix for webserver dynamic library issue (router_wrapper symbol)
    if [[ -d "/usr/local/lib" ]]; then
        log "Copying libraries from /usr/local/lib"
        mkdir -p "$ROOTFS/usr/local/lib"
        
        # Copy all shared libraries from /usr/local/lib
        cp -a /usr/local/lib/*.so* "$ROOTFS/usr/local/lib/" 2>/dev/null || 
            warn "Failed to copy libraries from /usr/local/lib"
    fi
    
    # Check for additional required libraries using ldd on the webserver binary
    if [[ -f "$ROOTFS/app/webserver" ]]; then
        log "Analyzing webserver binary dependencies"
        
        # Get required shared libraries
        if command -v ldd &>/dev/null; then
            ldd "$ROOTFS/app/webserver" 2>/dev/null | grep "=>" | awk '{print $3}' | while read -r lib; do
                if [[ -f "$lib" ]]; then
                    # Create directory structure if needed
                    lib_dir=$(dirname "$lib")
                    mkdir -p "$ROOTFS$lib_dir"
                    
                    # Copy the library
                    cp "$lib" "$ROOTFS$lib" 2>/dev/null || warn "Failed to copy $lib"
                fi
            done
            
            # Create ld.so.cache for the container
            mkdir -p "$ROOTFS/etc"
            if [[ -f "/etc/ld.so.conf" ]]; then
                cp /etc/ld.so.conf "$ROOTFS/etc/" 2>/dev/null || warn "Failed to copy ld.so.conf"
            else
                # Create a minimal ld.so.conf
                cat > "$ROOTFS/etc/ld.so.conf" << EOF
/lib
/usr/lib
/usr/local/lib
EOF
            fi
            
            # Create an ld.so.conf.d directory
            mkdir -p "$ROOTFS/etc/ld.so.conf.d"
            
            # Create a custom ld.so.conf.d entry for our libraries
            cat > "$ROOTFS/etc/ld.so.conf.d/cage-container.conf" << EOF
/usr/local/lib
/lib/x86_64-linux-gnu
/usr/lib/x86_64-linux-gnu
EOF
            
            # We can't run ldconfig inside the chroot yet, so we'll let the container handle it
        else
            warn "ldd command not found, unable to analyze library dependencies"
        fi
    fi
    
    # Handle network tool libraries specially
    for tool in socat nc netcat iptables ssh; do
        tool_path=$(which $tool 2>/dev/null)
        if [ -n "$tool_path" ] && command -v ldd &>/dev/null; then
            log "Copying libraries for $tool"
            ldd "$tool_path" 2>/dev/null | grep "=>" | awk '{print $3}' | while read -r lib; do
                if [[ -f "$lib" && ! -f "$ROOTFS$lib" ]]; then
                    # Create directory structure for library
                    lib_dir=$(dirname "$lib")
                    mkdir -p "$ROOTFS$lib_dir"
                    
                    # Copy the library
                    cp "$lib" "$ROOTFS$lib" 2>/dev/null || warn "Failed to copy $lib for $tool"
                fi
            done
        fi
    done
}

# Run the setup
setup_rootfs 