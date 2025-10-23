#!/bin/bash
# Slack Configuration Example
# Copy this file to slack_config.sh and update with your values

# Enable/disable Slack notifications
export SLACK_NOTIFICATIONS_ENABLED=true

# Your Slack webhook URL (replace with your actual webhook URL)
# Get this from: https://slack.com/apps/A0F7XDUAZ-incoming-webhooks
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

# Optional: Load this in your shell profile or startup scripts
# source /path/to/this/file