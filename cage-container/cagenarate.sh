#!/bin/bash

set -e

CONFIG_FILE="config.yaml"
NET_NAMESPACE="incredible-net"
NET_INTERFACE="veth0"
VETH_PEER="veth1"

ROOTFS=$(grep '^rootfs:' $CONFIG_FILE | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
CGROUP_PATH="$ROOTFS/sys/fs/cgroup"
PROJECT_PATH=$1

echo $PROJECT_PATH

NET_NAMESPACE=$(grep 'namespace:' $CONFIG_FILE | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
NET_IP=$(grep 'address:' $CONFIG_FILE | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
NET_INTERFACE=$(grep 'net_interface:' $CONFIG_FILE | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')
VETH_PEER=$(grep 'veth_peer:' $CONFIG_FILE | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')

if [[ -z "$NET_NAMESPACE" ]]; then
    echo "[ERROR] Network Namespace not found in config.yaml"
    exit 1
fi

if [[ -z "$NET_IP" ]]; then
    echo "[ERROR] Network IP not found in config.yaml"
    exit 1
fi

if [[ -z "$NET_INTERFACE" ]]; then
    echo "[ERROR] Network Interface not found in config.yaml"
    exit 1
fi

if [[ -z "$VETH_PEER" ]]; then
    echo "[ERROR] Network Veth Peer not found in config.yaml"
    exit 1
fi

MEMORY_LIMIT=$(grep -A3 'applications:' "$CONFIG_FILE" | grep 'memory:' | awk '{print $2}' | tr -d 'M' | tr -d '"' | tr -d '\n' | tr -d '\r')
CPU_LIMIT=$(grep -A3 'applications:' "$CONFIG_FILE" | grep 'cpu:' | awk '{print $2}' | tr -d '"' | tr -d '\n' | tr -d '\r')


if [[ -z "$MEMORY_LIMIT" ]]; then
    MEMORY_LIMIT=1024m
fi

if [[ -z "$CPU_LIMIT" ]]; then
    CPU_LIMIT=1
fi

MEMORY_BYTES=$((MEMORY_LIMIT * 1024 * 1024))
CPU_QUOTA=$((CPU_LIMIT * 100000))

IP_ONLY=$(echo "$NET_IP" | cut -d'/' -f1 | tr -d '\n' | tr -d '\r')
SUBNET_MASK=$(echo "$NET_IP" | cut -d'/' -f2 | tr -d '\n' | tr -d '\r')

if [[ -z "$IP_ONLY" || -z "$SUBNET_MASK" ]]; then
    echo "[ERROR] Invalid network configuration: $NET_IP"
    exit 1
fi

echo "[LOG] Setting up Incredible VM..."

if ip netns list | grep -q "$NET_NAMESPACE"; then
    echo "[LOG] Removing existing network namespace"
    ip netns del "$NET_NAMESPACE" || true
fi

if ip link show "$NET_INTERFACE" &> /dev/null; then
    echo "[LOG] Removing existing veth interface"
    ip link del "$NET_INTERFACE" || true
fi


echo "[LOG] Creating root filesystem at $ROOTFS"
fuser -k "$ROOTFS" || true
pkill -9 -f "$ROOTFS" || true
sync
sleep 1
mkdir -p "$ROOTFS/bin" "$ROOTFS/etc" "$ROOTFS/tmp" "$ROOTFS/proc" "$ROOTFS/logs" "$ROOTFS/usr/bin" "$ROOTFS/app"


cp /bin/sh "$ROOTFS/bin/"
cp /usr/bin/env "$ROOTFS/usr/bin/"
cp /usr/bin/cgexec "$ROOTFS/bin/"
mount -t proc proc "$ROOTFS/proc"

cp webserver/webserver "$ROOTFS/app/webserver"
chmod +x "$ROOTFS/app/webserver"

echo "[LOG] Configuring isolated network..."
ip netns add $NET_NAMESPACE
ip link add $NET_INTERFACE type veth peer name $VETH_PEER
ip link set $VETH_PEER netns $NET_NAMESPACE

ip addr add "$IP_ONLY/$SUBNET_MASK" dev $NET_INTERFACE || echo "[ERROR] Failed to assign IP to $NET_INTERFACE"
ip link set $NET_INTERFACE up
ip netns exec $NET_NAMESPACE ip addr add "$IP_ONLY/$SUBNET_MASK" dev $VETH_PEER || echo "[ERROR] Failed to assign IP inside namespace"
ip netns exec $NET_NAMESPACE ip link set $VETH_PEER up
ip netns exec $NET_NAMESPACE ip link set lo up

# setup libs
mkdir -p "$ROOTFS/lib/x86_64-linux-gnu"
mkdir -p "$ROOTFS/lib64"

#UPDATE HERE
# this probable won't work in other environment, update this to load this libreary from your system if you really want to run this shit
cp /lib/x86_64-linux-gnu/libtinfo.so.6 "$ROOTFS/lib/x86_64-linux-gnu/"
cp /lib/x86_64-linux-gnu/libc.so.6 "$ROOTFS/lib/x86_64-linux-gnu/"
cp /lib64/ld-linux-x86-64.so.2 "$ROOTFS/lib64/"
cp /usr/local/lib/libcob.so.4 "$ROOTFS/usr/lib/x86_64-linux-gnu/"
cp /usr/lib/x86_64-linux-gnu/libgmp.so.10 "$ROOTFS/usr/lib/x86_64-linux-gnu/"
cp /usr/lib/x86_64-linux-gnu/libdb-5.3.so "$ROOTFS/usr/lib/x86_64-linux-gnu/"

echo "[LOG] faking cgroup"
# Create a fake writable cgroup mount inside the rootfs
mkdir -p "$ROOTFS/sys/fs/cgroup"
mount -t tmpfs none "$ROOTFS/sys/fs/cgroup"

mkdir -p "$ROOTFS/sys/fs/cgroup/cpu/cage-container"
mkdir -p "$ROOTFS/sys/fs/cgroup/memory/cage-container"
mkdir -p "$ROOTFS/sys/fs/cgroup/pids/cage-container"

mount -t cgroup -o cpu none "$ROOTFS/sys/fs/cgroup/cpu"
mount -t cgroup -o memory none "$ROOTFS/sys/fs/cgroup/memory"
mount -t cgroup -o pids none "$ROOTFS/sys/fs/cgroup/pids"

mkdir -p "$ROOTFS/sys/fs/cgroup/cpu/cage-container"
mkdir -p "$ROOTFS/sys/fs/cgroup/memory/cage-container"
mkdir -p "$ROOTFS/sys/fs/cgroup/pids/cage-container"

touch "$ROOTFS/sys/fs/cgroup/cpu/cage-container/cgroup.procs"
touch "$ROOTFS/sys/fs/cgroup/memory/cage-container/cgroup.procs"
touch "$ROOTFS/sys/fs/cgroup/pids/cage-container/cgroup.procs"


echo "[LOG] Fake cgroup filesystem mounted inside Cage-Container"
if [ ! -f "/proc/self/ns/cgroup" ]; then
    echo "[ERROR] Your system does not support cgroup namespaces!"
    exit 1
fi

cp -r "$PROJECT_PATH" "$ROOTFS/app"

mksquashfs "$ROOTFS" /var/cage-container.img -comp xz -e proc sys dev run tmp || {
    echo "[ERROR] Failed to create SquashFS image"
    exit 1
}

echo "[LOG] Starting webserver..."
if [[ -f "$ROOTFS/webserver" ]]; then

    #chroot "$ROOTFS" /bin/sh -c "./webserver" | tee -a "$ROOTFS/logs/webserver.log" &
    if mount | grep -q "/mnt/cage-container"; then
        echo "[LOG] Unmounting previous SquashFS image..."
        umount /mnt/cage-container || true
        losetup -d /dev/loop0 || true

    fi
    mkdir -p /mnt/cage-container
    mount -o loop /var/cage-container.img /mnt/cage-container

    unshare --user --map-root-user --pid --fork --mount-proc chroot "/mnt/cage-container" /bin/sh -c "cd /app && ./webserver" &
    CAGE_PID=$!

    echo "$CAGE_PID" > "$ROOTFS/sys/fs/cgroup/cpu/cage-container/cgroup.procs"
    echo "$CAGE_PID" > "$ROOTFS/sys/fs/cgroup/memory/cage-container/cgroup.procs"
    echo "$CAGE_PID" > "$ROOTFS/sys/fs/cgroup/pids/cage-container/cgroup.procs"

    echo "[LOG] Webserver added to cge-container cgroups"
else
    echo "[ERROR] Webserver binary not found in $ROOTFS!"
    exit 1
fi

cleanup() {
    echo "[LOG] SIGINT (Ctrl+C), stopping ..."
    nsenter --target "$CAGE_PID" --pid -- kill -SIGINT -1
    sleep 1
    nsenter --target "$CAGE_PID" --pid -- kill -9 -1
    echo "[LOG] Cage-Container stopped."
    exit 0
}
trap cleanup SIGINT

echo "[LOG] cage-container is running"


tail -f "$ROOTFS/logs/webserver.log"
