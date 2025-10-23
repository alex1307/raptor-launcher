#!/usr/bin/env bash
set -e
exec >> "$RAPTOR_HOME/logs/raptor.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Stopping Raptor..."

pkill -f "make start" || true

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Raptor stopped."