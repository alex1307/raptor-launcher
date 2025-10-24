#!/usr/bin/env bash
# Uninstall raptor-launcher systemd service from Ubuntu

set -e

SYSTEMD_DIR="/etc/systemd/system"
SERVICE_FILE="$SYSTEMD_DIR/raptor-launcher.service"

# Check if running as root
if [ "$EUID" -ne 0 ]; then 
    echo "Please run with sudo: sudo $0"
    exit 1
fi

# Stop service if running
echo "Stopping raptor-launcher service..."
systemctl stop raptor-launcher.service || true

# Disable service
echo "Disabling raptor-launcher service..."
systemctl disable raptor-launcher.service || true

# Remove service file
if [ -f "$SERVICE_FILE" ]; then
    echo "Removing service file..."
    rm -f "$SERVICE_FILE"
fi

# Reload systemd
echo "Reloading systemd daemon..."
systemctl daemon-reload

echo ""
echo "✅ Uninstallation complete!"
echo ""
