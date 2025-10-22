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
```

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

- `start/0` - Start HTTP client for Slack service
- `stop/0` - Stop HTTP client
- `is_enabled/0` - Check if notifications are enabled
- `notify/1` - Send basic text message
- `notify_info/1` - Send info message with ℹ️ emoji
- `notify_warning/1` - Send warning message with ⚠️ emoji  
- `notify_error/1` - Send error message with ❌ emoji
- `notify_success/1` - Send success message with ✅ emoji

### Return Values

All notification functions return:

- `ok` - Message sent successfully or notifications disabled
- `{error, Reason}` - Failed to send message

### Error Handling

The service handles various error conditions gracefully:

- **Disabled notifications**: Returns `ok` without attempting to send
- **Missing webhook URL**: Logs warning and returns `{error, not_configured}`
- **Network errors**: Logs error and returns `{error, {http_request_failed, Reason}}`
- **Slack API errors**: Logs error with status code and returns `{error, {http_error, Code, Reason}}`

## Best Practices

1. **Always start the service**: Call `slack_utils:start()` during application initialization
2. **Use typed messages**: Use `notify_info/1`, `notify_error/1`, etc. for better visual distinction
3. **Wrap in try-catch**: For critical code paths, wrap Slack calls to prevent crashes
4. **Configure appropriately**: Only enable in production or staging environments
5. **Meaningful messages**: Include context and relevant information in notifications

## Example Configuration Script

See `config/slack_config_example.sh` for an example configuration script.