#!/usr/bin/env bash
set -e
exec >> "$MOBILE_DE_HOME/logs/mobile_de.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Stopping mobile_de..."

pkill -f "mobile_de" || true

echo "[$(date '+%Y-%m-%d %H:%M:%S')] mobile_de stopped."