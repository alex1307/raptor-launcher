#!/bin/bash
set -e

#############################################
# Auto-update script for raptor-launcher
# Checks for Git changes, rebuilds, and deploys
#############################################

REPO_DIR="/home/matkat/raptor-launcher"
SLACK_WEBHOOK="${SLACK_WEBHOOK_URL}"
SERVICE_NAME="raptor-launcher"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1"
}

error() {
    echo -e "${RED}[$(date +'%Y-%m-%d %H:%M:%S')] ERROR:${NC} $1"
}

warn() {
    echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')] WARNING:${NC} $1"
}

send_slack() {
    local status="$1"
    local message="$2"
    
    if [ -z "$SLACK_WEBHOOK" ]; then
        warn "SLACK_WEBHOOK_URL not set, skipping notification"
        return
    fi
    
    local emoji="✅"
    [ "$status" = "error" ] && emoji="❌"
    [ "$status" = "warning" ] && emoji="⚠️"
    
    local payload=$(cat <<EOF
{
    "text": "${emoji} Raptor Launcher Auto-Update\n${message}"
}
EOF
)
    
    curl -X POST -H 'Content-type: application/json' \
         --data "$payload" \
         "$SLACK_WEBHOOK" 2>/dev/null || warn "Failed to send Slack notification"
}

# Change to repo directory
cd "$REPO_DIR" || {
    error "Failed to cd to $REPO_DIR"
    send_slack "error" "Failed to access repository directory"
    exit 1
}

# Fetch latest changes
log "Fetching latest changes from origin..."
git fetch origin main 2>&1 || {
    error "Git fetch failed"
    send_slack "error" "Git fetch failed"
    exit 1
}

# Check if there are new commits
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse @{u})

if [ "$LOCAL" = "$REMOTE" ]; then
    log "No updates available. Already up to date."
    exit 0
fi

# Get commit messages for Slack
COMMIT_MESSAGES=$(git log --oneline $LOCAL..$REMOTE | head -5)
log "New commits found:\n$COMMIT_MESSAGES"

send_slack "info" "New commits detected. Starting update...\n\`\`\`\n${COMMIT_MESSAGES}\n\`\`\`"

# Pull latest changes
log "Pulling latest changes..."
git pull origin main || {
    error "Git pull failed"
    send_slack "error" "Git pull failed"
    exit 1
}

# Stop service
log "Stopping $SERVICE_NAME service..."
sudo systemctl stop "$SERVICE_NAME" || {
    error "Failed to stop service"
    send_slack "error" "Failed to stop service"
    exit 1
}

# Backup current deployment
BACKUP_DIR="launcher-app.backup.$(date +%Y%m%d_%H%M%S)"
log "Backing up current deployment to $BACKUP_DIR..."
if [ -d "launcher-app" ]; then
    mv launcher-app "$BACKUP_DIR"
fi

# Build release
log "Building production release..."
if ! make deploy 2>&1 | tee /tmp/raptor-build.log; then
    error "Build failed!"
    send_slack "error" "Build failed! Check logs on server."
    # Restore backup
    [ -d "$BACKUP_DIR" ] && mv "$BACKUP_DIR" launcher-app
    sudo systemctl start "$SERVICE_NAME"
    exit 1
fi

# Copy environment files from backup
if [ -d "$BACKUP_DIR/devops/env" ]; then
    log "Restoring environment configuration..."
    cp "$BACKUP_DIR/devops/env/.systemd.env" launcher-app/devops/env/ 2>/dev/null || true
    cp "$BACKUP_DIR/devops/env/raptor.systemd.env" launcher-app/devops/env/ 2>/dev/null || true
fi

# Reinstall service (in case service file changed)
log "Reinstalling systemd service..."
cd launcher-app/devops
sudo ./install-service.sh >/dev/null 2>&1 || warn "Service install had warnings"

# Start service
log "Starting $SERVICE_NAME service..."
sudo systemctl start "$SERVICE_NAME" || {
    error "Failed to start service"
    send_slack "error" "Service failed to start after update!"
    exit 1
}

# Wait a bit and check status
sleep 3
if sudo systemctl is-active --quiet "$SERVICE_NAME"; then
    log "Service started successfully!"
    NEW_VERSION=$(git rev-parse --short HEAD)
    send_slack "success" "Update completed successfully! Now running commit: \`${NEW_VERSION}\`\n\`\`\`\n${COMMIT_MESSAGES}\n\`\`\`"
else
    error "Service is not running!"
    send_slack "error" "Service failed health check after update!"
    exit 1
fi

# Cleanup old backups (keep last 3)
log "Cleaning up old backups..."
cd "$REPO_DIR"
ls -dt launcher-app.backup.* 2>/dev/null | tail -n +4 | xargs rm -rf 2>/dev/null || true

log "Auto-update completed successfully!"
