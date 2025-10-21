#!/usr/bin/env bash
set -e

OS_TYPE="$(uname)"
if [[ "$OS_TYPE" == "Darwin" ]]; then
  CRAWLER_HOME="${MAC_CRAWLER_HOME:-/Users/matkat/Software/docker-env/crawler}"
elif [[ "$OS_TYPE" == "Linux" ]]; then
  CRAWLER_HOME="${UBUNTU_CRAWLER_HOME:-/home/matkat/crawler-app}"
else
  echo "Unsupported OS: $OS_TYPE"
  exit 1
fi

cd "$CRAWLER_HOME"

exec >> "$CRAWLER_HOME/_logs/mobile_de.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Stopping mobile_de..."

pkill -f "mobile_de" || true

echo "[$(date '+%Y-%m-%d %H:%M:%S')] mobile_de stopped."