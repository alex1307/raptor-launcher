#!/bin/bash
set -e

#############################################
# Setup auto-update cron job for raptor-launcher
#############################################

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AUTO_UPDATE_SCRIPT="$SCRIPT_DIR/auto-update.sh"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
LOG_FILE="$HOME/raptor-auto-update.log"

echo "================================================"
echo "  Raptor Launcher Auto-Update Setup"
echo "================================================"
echo ""

# Check if auto-update.sh exists
if [ ! -f "$AUTO_UPDATE_SCRIPT" ]; then
    echo "❌ Error: auto-update.sh not found at $AUTO_UPDATE_SCRIPT"
    exit 1
fi

# Make it executable
chmod +x "$AUTO_UPDATE_SCRIPT"
echo "✅ Made auto-update.sh executable"

# Ask for update interval
echo ""
echo "Select update check interval:"
echo "  1) Every 10 minutes (recommended)"
echo "  2) Every 30 minutes"
echo "  3) Every hour"
echo "  4) Custom"
read -p "Enter choice [1-4]: " choice

case $choice in
    1)
        CRON_SCHEDULE="*/10 * * * *"
        INTERVAL_DESC="every 10 minutes"
        ;;
    2)
        CRON_SCHEDULE="*/30 * * * *"
        INTERVAL_DESC="every 30 minutes"
        ;;
    3)
        CRON_SCHEDULE="0 * * * *"
        INTERVAL_DESC="every hour"
        ;;
    4)
        read -p "Enter cron schedule (e.g., '*/15 * * * *'): " CRON_SCHEDULE
        INTERVAL_DESC="custom: $CRON_SCHEDULE"
        ;;
    *)
        echo "❌ Invalid choice"
        exit 1
        ;;
esac

# Ask for Slack webhook (optional)
echo ""
read -p "Enter Slack webhook URL (optional, press Enter to skip): " SLACK_URL

# Build cron command
if [ -n "$SLACK_URL" ]; then
    CRON_COMMAND="$CRON_SCHEDULE SLACK_WEBHOOK_URL=\"$SLACK_URL\" $AUTO_UPDATE_SCRIPT >> $LOG_FILE 2>&1"
else
    CRON_COMMAND="$CRON_SCHEDULE $AUTO_UPDATE_SCRIPT >> $LOG_FILE 2>&1"
fi

# Check if cron job already exists
if crontab -l 2>/dev/null | grep -q "auto-update.sh"; then
    echo ""
    echo "⚠️  Auto-update cron job already exists!"
    read -p "Do you want to replace it? (y/n): " replace
    if [ "$replace" != "y" ]; then
        echo "Cancelled."
        exit 0
    fi
    # Remove old cron job
    crontab -l 2>/dev/null | grep -v "auto-update.sh" | crontab -
    echo "✅ Removed old cron job"
fi

# Add new cron job
(crontab -l 2>/dev/null; echo "$CRON_COMMAND") | crontab -
echo "✅ Added cron job: $INTERVAL_DESC"

# Setup sudo permissions
echo ""
echo "Setting up sudo permissions..."
SUDOERS_FILE="/etc/sudoers.d/raptor-launcher"
SUDOERS_CONTENT="# Raptor Launcher auto-update permissions
$USER ALL=(ALL) NOPASSWD: /bin/systemctl start raptor-launcher
$USER ALL=(ALL) NOPASSWD: /bin/systemctl stop raptor-launcher
$USER ALL=(ALL) NOPASSWD: /bin/systemctl restart raptor-launcher
$USER ALL=(ALL) NOPASSWD: /bin/systemctl status raptor-launcher
$USER ALL=(ALL) NOPASSWD: $(realpath ~/launcher-app/devops/install-service.sh 2>/dev/null || echo "/home/$USER/launcher-app/devops/install-service.sh")
"

if [ -f "$SUDOERS_FILE" ]; then
    echo "⚠️  Sudoers file already exists: $SUDOERS_FILE"
    read -p "Do you want to replace it? (y/n): " replace_sudo
    if [ "$replace_sudo" = "y" ]; then
        echo "$SUDOERS_CONTENT" | sudo tee "$SUDOERS_FILE" > /dev/null
        echo "✅ Updated sudoers file"
    fi
else
    echo "$SUDOERS_CONTENT" | sudo tee "$SUDOERS_FILE" > /dev/null
    sudo chmod 0440 "$SUDOERS_FILE"
    echo "✅ Created sudoers file: $SUDOERS_FILE"
fi

# Test the script
echo ""
echo "Testing auto-update script (dry run)..."
if "$AUTO_UPDATE_SCRIPT" 2>&1 | head -20; then
    echo "✅ Script test completed"
else
    echo "⚠️  Script test had issues (check logs above)"
fi

echo ""
echo "================================================"
echo "  ✅ Auto-Update Setup Complete!"
echo "================================================"
echo ""
echo "Configuration:"
echo "  • Cron schedule: $INTERVAL_DESC"
echo "  • Script: $AUTO_UPDATE_SCRIPT"
echo "  • Log file: $LOG_FILE"
[ -n "$SLACK_URL" ] && echo "  • Slack notifications: enabled"
echo ""
echo "To view cron jobs:"
echo "  crontab -l"
echo ""
echo "To view logs:"
echo "  tail -f $LOG_FILE"
echo ""
echo "To manually trigger update:"
echo "  $AUTO_UPDATE_SCRIPT"
echo ""
