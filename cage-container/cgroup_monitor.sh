#!/bin/bash

CGROUP_PATH="$ROOTFS/sys/fs/cgroup"

while true; do
    echo "[LOG] Checking cgroup limits..."

    echo 50000 > "$CGROUP_PATH/cpu/cpu.cfs_quota_us"
    echo 134217728 > "$CGROUP_PATH/memory/memory.limit_in_bytes"
    echo 50 > "$CGROUP_PATH/pids/pids.max"

    echo "[LOG] cgroup limits applied."
    sleep 5
done
