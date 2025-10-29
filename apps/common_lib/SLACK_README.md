# Slack Notification Service

This module provides a robust Slack notification service for the Raptor Launcher application.

## Features

- ✅ Environment-based configuration
- ✅ Automatic HTTP client management
- ✅ Proper error handling and logging
- ✅ Message type helpers with emojis
- ✅ JSON payload encoding
- ✅ Graceful degradation when disabled

## Configuration

### Environment Variables

Set these environment variables to configure Slack notifications:

```bash
# Enable notifications
export SLACK_NOTIFICATIONS_ENABLED=true

# Your Slack webhook URL
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

# Rate limiting (optional, default: 10)
# Maximum messages per minute to prevent 429 errors
export SLACK_MAX_MESSAGES_PER_MINUTE=10
```

### Rate Limiting

To prevent Slack's 429 "Too Many Requests" error, the module implements automatic rate limiting:

- Default: **10 messages per minute**
- Configurable via `SLACK_MAX_MESSAGES_PER_MINUTE` environment variable
- Messages exceeding the limit are **dropped** with a warning in logs
- Rate limit window resets every 60 seconds

**Example**: If you have 20 services running and each fails 3 times, that's 60 potential messages. With rate limiting set to 10/minute, only the first 10 will be sent, preventing Slack API blocking.

### Getting a Webhook URL

1. Go to your Slack workspace
2. Navigate to Apps → Incoming Webhooks
3. Click "Add to Slack"
4. Choose the channel for notifications
5. Copy the webhook URL

## Usage

### Basic Usage

```erlang
%% Start the service (usually done at application startup)
slack_utils:start().

%% Send a simple message
slack_utils:notify("Application started successfully").

%% Send typed messages with emojis
slack_utils:notify_info("System information").
slack_utils:notify_warning("Potential issue detected").
slack_utils:notify_error("Critical error occurred").
slack_utils:notify_success("Operation completed").
```

### Integration Example

```erlang
%% In your application supervisor or main module
init([]) ->
    %% Start Slack service
    slack_utils:start(),
    
    %% Send startup notification
    slack_utils:notify_success("🚀 Raptor Launcher started"),
    
    %% Continue with your initialization
    {ok, ...}.
```

### Safe Usage in FSMs/GenServers

```erlang
%% Safe wrapper that doesn't crash on errors
safe_slack_notify(Message) ->
    try
        slack_utils:notify(Message)
    catch
        _:Error ->
            lager:warning("Slack notification failed: ~p", [Error]),
            ok
    end.
```

## API Reference

### Functions

- `start/0` - Start HTTP client and initialize rate limiter
- `stop/0` - Stop HTTP client and cleanup rate limiter
- `reset_rate_limiter/0` - Reset rate limiter (for testing or manual reset)
- `is_enabled/0` - Check if notifications are enabled
- `notify/1` - Send basic text message
- `notify_info/1` - Send info message with ℹ️ emoji
- `notify_warning/1` - Send warning message with ⚠️ emoji  
- `notify_error/1` - Send error message with ❌ emoji
- `notify_success/1` - Send success message with ✅ emoji

### Return Values

All notification functions return:

- `ok` - Message sent successfully or notifications disabled
- `{error, rate_limited}` - Message dropped due to rate limiting
- `{error, Reason}` - Failed to send message

### Error Handling

The service handles various error conditions gracefully:

- **Disabled notifications**: Returns `ok` without attempting to send
- **Rate limiting**: Returns `{error, rate_limited}` and logs warning when limit exceeded
- **Missing webhook URL**: Logs warning and returns `{error, not_configured}`
- **Network errors**: Logs error and returns `{error, {http_request_failed, Reason}}`
- **Slack API errors**: Logs error with status code and returns `{error, {http_error, Code, Reason}}`

## Best Practices

1. **Always start the service**: Call `slack_utils:start()` during application initialization
2. **Use typed messages**: Use `notify_info/1`, `notify_error/1`, etc. for better visual distinction
3. **Configure rate limit**: Adjust `SLACK_MAX_MESSAGES_PER_MINUTE` based on your needs:
   - **Low traffic**: 5-10 messages/minute  
   - **Medium traffic**: 15-20 messages/minute
   - **High traffic**: Consider batching or aggregating messages
4. **Remove retry spam**: Don't send Slack messages on every retry attempt, only on final failure
5. **Aggregate notifications**: Send one summary per batch/run instead of per-item messages

## Troubleshooting

### 429 Too Many Requests Error

If you see `429 Too Many Requests` errors or get blocked by Slack:

1. **Reduce rate limit**: Lower `SLACK_MAX_MESSAGES_PER_MINUTE` (try 5-10)
2. **Find message sources**: Search code for all `slack_utils:notify*` calls
3. **Remove retry notifications**: Ensure FSMs/services don't send on every retry
4. **Batch notifications**: Aggregate multiple events into single messages
5. **Check logs**: `grep "rate limited" log/console.log`

### Messages Not Sending

1. Check `SLACK_NOTIFICATIONS_ENABLED=true` is set
2. Verify webhook URL is correct and not revoked
3. Check logs for HTTP errors or rate limiting warnings
4. Ensure `inets` application is started

## Example Configuration Script

See `config/slack_config_example.sh` for an example configuration script.
