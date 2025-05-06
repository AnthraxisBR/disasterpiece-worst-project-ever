# Cage Container

A lightweight container simulation using Linux namespaces and cgroups.

## Overview

Cage Container simulates a container environment using the following Linux features:
- **Namespace isolation**: PID, UTS, mount, network, and user namespaces for process isolation
- **Resource limits**: CPU, memory, and process count limits using cgroups
- **Filesystem isolation**: SquashFS for read-only filesystem with overlays
- **Network isolation**: Virtual Ethernet pairs and network namespaces
- **Port forwarding**: For accessing container services from the host

## Requirements

- Linux with namespace and cgroup support
- Root privileges
- The following tools:
  - ip (for network configuration)
  - unshare (for namespace isolation)
  - mount/umount (for filesystem operations)
  - mksquashfs (for creating SquashFS images)
  - chroot (for filesystem isolation)
  - nsenter (for container operations)
  - iptables/socat/netcat (for port forwarding, at least one required)

## Usage

### Configuration

Edit the `config.yaml` file to configure your container:

```yaml
rootfs: /var/cage-container
network:
  namespace: cage-container
  address: 10.100.0.2/16
  net_interface: veth0
  veth_peer: veth1
applications:
  - type: sh
    command: "./webserver"
    limits:
      memory: 128M
      cpu: 0.5
  - type: sh
    command: "./cgroup_monitor.sh"
```

### Starting the Container

```bash
sudo ./cage-container.sh [OPTIONS] PROJECT_PATH
```

Options:
- `-c, --config FILE`: Path to config file (default: config.yaml)
- `-p, --port PORT`: Port to expose on host (maps to port 8080 in container)
- `--stop`: Stop running container
- `--status`: Show status of running container
- `--ps`: List processes in the container
- `--exec COMMAND`: Execute command in running container
- `--reset`: Force reset all container resources (use when stuck)
- `-h, --help`: Show help message

Example:
```bash
# Start container with port forwarding
sudo ./cage-container.sh -p 8080 /path/to/your/application

# Access the webserver at http://localhost:8080
```

### Managing Containers

#### Stopping the Container

```bash
sudo ./cage-container.sh --stop
```

#### Force Cleanup (when normal stop doesn't work)

If the container is in an inconsistent state or you encounter mount point busy errors, use one of these options:

```bash
# Normal cleanup with enhanced error handling
sudo ./cage-container.sh --stop

# Complete reset of all container resources (use when normal cleanup fails)
sudo ./cage-container.sh --reset

# Last resort - use cleanup-all script
sudo ./cleanup-all.sh
```

The `--reset` option will:
- Kill all related processes
- Unmount all filesystems (with force options if needed)
- Detach all loop devices
- Remove network namespaces and interfaces
- Clean up iptables rules
- Remove temporary files

Use this command before starting a new container if you've experienced issues.

#### Checking Container Status

```bash
sudo ./cage-container.sh --status
```

This will display:
- Whether a container is running
- Container PID and start time
- Resource usage (CPU, memory)
- Network information
- Port forwarding details

#### Listing Container Processes

```bash
sudo ./cage-container.sh --ps
```

This command works independently of container start parameters and will show all processes running inside the container.

#### Executing Commands in the Container

```bash
sudo ./cage-container.sh --exec "COMMAND"
```

For example:
```bash
sudo ./cage-container.sh --exec "ls -la /app"
```

## Structure

- `cage-container.sh`: Main entry point script
- `lib/common.sh`: Common functions and variables
- `lib/setup_rootfs.sh`: Root filesystem setup
- `lib/setup_network.sh`: Network isolation setup
- `lib/setup_cgroups.sh`: Cgroup resource limits setup
- `lib/start_container.sh`: Container startup script
- `lib/setup_port_forward.sh`: Port forwarding setup
- `lib/cleanup.sh`: Container cleanup script
- `lib/container_status.sh`: Container status inspection
- `cgroup_monitor.sh`: Resource monitoring script
- `cleanup-all.sh`: Force cleanup script for resolving stuck resources
- `reset.sh`: Complete environment reset script (useful for WSL environments)
- `test-container.sh`: Test script to verify container functionality

## Inside the Container

The container provides:
- Resource limits (CPU, memory, PIDs)
- Network isolation
- Filesystem isolation
- Basic monitoring via cgroup_monitor.sh

## Extending

You can extend the container functionality by:
1. Adding volume mounts
2. Adding more cgroup controllers
3. Enhancing the network setup with port forwarding
4. Implementing user namespaces more comprehensively

## Troubleshooting

Logs are available in:
- Host: `cage-container/logs/container.log`
- Container: `/logs/cgroup_monitor.log`

### Common Issues and Solutions

1. **Container Not Accessible from Host**
   - Ensure port forwarding is set up with the `-p` option
   - Check if iptables, socat, or netcat is installed (socat is most reliable)
   - Verify that the webserver inside the container is running on port 8080
   - Try different port forwarding methods by editing `lib/setup_port_forward.sh`
   - Check logs at `logs/socat.log` or `logs/nc.log` for connection issues

2. **Container Doesn't Stop with Ctrl+C**
   - If Ctrl+C doesn't stop the container, use `sudo ./cage-container.sh --stop`
   - If that fails, try the reset option: `sudo ./cage-container.sh --reset`
   - For persistent issues, check if processes are still running with `ps aux | grep cage`
   - Kill any remaining processes manually with `kill -9 PID`

3. **Status and Process Commands Not Working**
   - These commands have been improved to work independently of container state
   - If they show "No container is running", the container process is genuinely not running
   - If commands show stale PID information, run `sudo ./cage-container.sh --reset` to clean up
   - Check for error messages in `logs/container.log`

4. **"Mount Point Busy" or "Already Mounted" Errors**
   - The cleanup script now tries lazy unmount when normal unmount fails
   - Use `sudo ./cage-container.sh --reset` which aggressively cleans up resources
   - If issues persist, check what's still mounted with `mount | grep cage`
   - For stubborn mounts, try `sudo umount -l /path/to/mount` manually

5. **Container Starts But No Web Access**
   - Port forwarding now tries multiple methods (socat, iptables, netcat, ssh)
   - Try accessing directly with curl: `curl http://localhost:PORT`
   - Check which port forwarding method is being used in `logs/port_forward.method`
   - Manually try port forwarding with: `socat TCP-LISTEN:8080,fork TCP:10.100.0.2:8080`
   - Verify container networking with: `sudo ./cage-container.sh --exec "ping 8.8.8.8"`

6. **Missing Commands or Libraries in Container**
   - If you see errors like "command not found" or "symbol lookup error", your container is missing essential tools or libraries
   - The rootfs setup has been improved to include common utilities
   - Debug by executing a shell in the container: `sudo ./cage-container.sh --exec "/bin/sh"`
   - For dynamic library issues, check paths with: `sudo ./cage-container.sh --exec "ldd /app/webserver"`
   - Modify `lib/setup_rootfs.sh` to include more tools or libraries as needed

7. **General Troubleshooting Steps**
   - Check log files for detailed errors: `cat cage-container/logs/container.log`
   - Run the test script to verify functionality: `sudo ./cage-container/test-container.sh`
   - Run the reset command: `sudo ./cage-container.sh --reset`
   - Ensure all required tools are installed
   - Verify you have root privileges
   - Make sure cgroups and namespaces are supported on your kernel

8. **Cgroup Issues**
   - The system now supports both cgroup v1 and v2 formats
   - If you see cgroup errors, check which version your system uses: `ls -l /sys/fs/cgroup/`
   - For cgroup v1, you should see subdirectories like cpu, memory
   - For cgroup v2, you'll see a unified hierarchy
   - Adjust cgroup paths in `lib/setup_cgroups.sh` if needed

9. **Running in WSL (Windows Subsystem for Linux)**
   - WSL has limitations with network namespaces and cgroups
   - Use the reset script when operations fail: `sudo ./reset.sh`
   - If port forwarding fails, try using socat manually outside WSL
   - Some namespace operations may require the latest WSL2 kernel

## License

This project is licensed under the MIT License - see the LICENSE file for details. 