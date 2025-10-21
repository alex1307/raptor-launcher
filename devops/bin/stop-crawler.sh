#!/usr/bin/env bash
set -e
exec >> "$CRAWLER_HOME/_logs/crawler.log" 2>&1

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Stopping crawler..."

pkill -f "crawler" || true

echo "[$(date '+%Y-%m-%d %H:%M:%S')] crawler stopped."