#!/usr/bin/env bash
set -euo pipefail

# Detect OS and select Chrome binary from environment (defined via YAML)
OS_TYPE="$(uname)"
if [[ "$OS_TYPE" == "Darwin" ]]; then
  CHROME_BIN="${CHROME_BIN_MAC:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
elif [[ "$OS_TYPE" == "Linux" ]]; then
  CHROME_BIN="${CHROME_BIN_UBUNTU:-/usr/bin/google-chrome}"
    # Check if X server is running; if not, start a virtual one
  if ! pgrep -x "Xorg" > /dev/null && ! pgrep -x "X" > /dev/null && ! pgrep -x "xinit" > /dev/null; then
    echo "No X server detected. Starting virtual X server (Xvfb $DISPLAY)..."
    nohup Xvfb $DISPLAY -screen 0 1920x1080x24 > /tmp/xvfb.log 2>&1 &
    sleep 2
  else
    echo "X server already running."
  fi
else
  echo "Unsupported OS: $OS_TYPE"
  exit 1
fi

echo "Detected OS: $OS_TYPE"
echo "Using Chrome binary: $CHROME_BIN"
echo "Display: $DISPLAY"
echo "XAUTHORITY: $XAUTHORITY"

# Create base directory for Chrome sessions
BASE="$HOME/chrome/tmp/sessions"
mkdir -p "$BASE/chrome/tmp"
# Redirect stdout/stderr to log file (important for non-interactive service)
exec >> "$BASE/chrome/tmp/chrome.log" 2>&1
# Fresh per-run Chrome profile

rm -rf "$BASE"
SESSION="$BASE/session_$(date +%s)"
mkdir -p "$SESSION"

# Start Chrome with remote debugging on 9223
# NOTE: Run this script normally (blocks) or in background with `&` if you want the shell back.
# Added flags to reduce background CPU usage and disable non-essential services.

echo "Starting Chrome with remote debugging on port 9223..."
echo "Chrome Binary: $CHROME_BIN"

exec "$CHROME_BIN" \
  --remote-debugging-port=9223 \
  --user-data-dir="$SESSION" \
  --ignore-certificate-errors \
  --no-first-run \
  --no-default-browser-check \
  --disable-dev-shm-usage \
  --start-maximized \
  --disable-background-networking \
  --disable-background-timer-throttling \
  --disable-renderer-backgrounding \
  --disable-backgrounding-occluded-windows \
  --disable-extensions \
  --mute-audio \
  --autoplay-policy=no-user-gesture-required \
  --disable-sync \
  --disable-client-side-phishing-detection \
  --disable-component-update \
  --disable-default-apps \
  --disable-domain-reliability \
  --disable-features=Translate,BackForwardCache,MediaSessionService