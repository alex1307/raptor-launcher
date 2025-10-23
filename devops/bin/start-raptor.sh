#!/usr/bin/env bash
set -e

# check parameter
if [ -z "$1" ]; then
  echo "Usage: $0 --site <site>"
  echo "Example: $0 mobile.de"
  exit 1
fi

SOURCE="$1"

OS_TYPE="$(uname)"
if [[ "$OS_TYPE" == "Darwin" ]]; then
  RAPTOR_HOME="${MAC_RAPTOR_HOME:-/Users/matkat/Software/JavaScript/mobile-crawler}"
elif [[ "$OS_TYPE" == "Linux" ]]; then
  RAPTOR_HOME="${UBUNTU_RAPTOR_HOME:-/home/matkat/mobile-crawler}"
else
  echo "Unsupported OS: $OS_TYPE"
  exit 1
fi  

cd "$RAPTOR_HOME"
mkdir -p logs

exec >> "$RAPTOR_HOME/logs/raptor.log" 2>&1
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting Raptor for $SOURCE..."
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting Raptor 2.0 from $(pwd)..."
nohup make start >> ./logs/raptor.log 2>&1 &

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Raptor started."