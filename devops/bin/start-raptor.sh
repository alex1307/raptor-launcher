#!/usr/bin/env bash
set -e

# check parameter
if [ -z "$1" ]; then
  echo "Usage: $0 --site <site>"
  echo "Example: $0 mobile.de"
  exit 1
fi

SOURCE="$1"

cd "$RAPTOR_HOME"
mkdir -p logs

exec >> "$RAPTOR_HOME/logs/raptor.log" 2>&1
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting Raptor for $SOURCE..."
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting Raptor 2.0 from $(pwd)..."
nohup node src/boot/launcher.js --site $SOURCE >> ./logs/raptor.log 2>&1 &

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Raptor started."