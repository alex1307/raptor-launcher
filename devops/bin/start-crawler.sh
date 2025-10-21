#!/usr/bin/env bash
set -e

# check parameter
if [ -z "$1" ]; then
  echo "Usage: $0 --source <site>"
  echo "Example: $0 autouncle.ro"
  exit 1
fi

SOURCE="$1"

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
mkdir -p _logs

exec >> "$CRAWLER_HOME/_logs/crawler.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting crawler for source '$SOURCE'..."
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Working directory: $(pwd)..."

nohup ./crawler --source "$SOURCE" >> ./_logs/crawler.log 2>&1 &

echo "[$(date '+%Y-%m-%d %H:%M:%S')] crawler for '$SOURCE' started."