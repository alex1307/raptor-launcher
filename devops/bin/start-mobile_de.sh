#!/usr/bin/env bash
set -e

cd "$MOBILE_DE_HOME"
mkdir -p _logs

exec >> "$MOBILE_DE_HOME/_logs/mobile_de.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting mobile_de..."
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting mobile_de from $(pwd)..."

nohup ./mobile_de >> ./_logs/mobile_de.log 2>&1 &

echo "[$(date '+%Y-%m-%d %H:%M:%S')] mobile_de started."