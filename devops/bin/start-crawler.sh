#!/usr/bin/env bash
set -e

# check parameter
if [ -z "$1" ]; then
  echo "Usage: $0 --source <site>"
  echo "Example: $0 autouncle.ro"
  exit 1
fi

SOURCE="$1"

cd "$MOBILE_DE_HOME"
mkdir -p _logs

exec >> "$MOBILE_DE_HOME/_logs/crawler.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting crawler for source '$SOURCE'..."
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Working directory: $(pwd)..."

nohup ./crawler --source "$SOURCE" >> ./_logs/crawler.log 2>&1 &

echo "[$(date '+%Y-%m-%d %H:%M:%S')] crawler for '$SOURCE' started."