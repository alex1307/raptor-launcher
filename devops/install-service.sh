#!/usr/bin/env bash
# Install raptor-launcher as systemd service on Ubuntu

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVICE_FILE="$SCRIPT_DIR/raptor-launcher.service"
SYSTEMD_DIR="/etc/systemd/system"

# Check if running as root
if [ "$EUID" -ne 0 ]; then 
    echo "Please run with sudo: sudo $0"
    exit 1
fi

# Copy service file
echo "Installing raptor-launcher.service to $SYSTEMD_DIR..."
cp "$SERVICE_FILE" "$SYSTEMD_DIR/raptor-launcher.service"

# Reload systemd
echo "Reloading systemd daemon..."
systemctl daemon-reload

# Enable service to start on boot
echo "Enabling raptor-launcher service..."
systemctl enable raptor-launcher.service

echo ""
echo "✅ Installation complete!"
echo ""
echo "Usage:"
echo "  sudo systemctl start raptor-launcher    # Start service"
echo "  sudo systemctl stop raptor-launcher     # Stop service"
echo "  sudo systemctl restart raptor-launcher  # Restart service"
echo "  sudo systemctl status raptor-launcher   # Check status"
echo "  sudo journalctl -u raptor-launcher -f   # View logs (follow)"
echo ""
