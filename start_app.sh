#!/bin/bash
set -e

# Determine the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LAUNCHER_APP_DIR="$(dirname "$SCRIPT_DIR")"

# Load environment variables
set -a
[ -f "$LAUNCHER_APP_DIR/devops/env/raptor.systemd.env" ] && source "$LAUNCHER_APP_DIR/devops/env/raptor.systemd.env"
[ -f "$LAUNCHER_APP_DIR/devops/env/.systemd.env" ] && source "$LAUNCHER_APP_DIR/devops/env/.systemd.env"
set +a

# Start Erlang release
exec "$LAUNCHER_APP_DIR/raptor_launcher/bin/raptor_launcher" foreground