#!/bin/bash

APP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$APP_DIR"

set -a
source devops/env/raptor.systemd.env
source devops/env/.systemd.env
set +a

exec raptor_launcher/bin/raptor_launcher foreground