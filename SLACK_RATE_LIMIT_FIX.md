# Slack Rate Limiting Fix

## Problem

The application was sending **too many Slack messages** and hitting Slack's rate limit, resulting in **429 "Too Many Requests"** errors and temporary blocking.

### Root Causes

1. **FSM Retry Spam**: `raptor_service_fsm.erl` was sending a Slack notification on **every retry attempt**
   - Example: 10 services × 3 retry attempts = **30 messages** on failures
   - Function: `send_slack_error/3` was called in line 105

2. **Scheduler Summary Spam**: `raptor_scheduler.erl` sends summary after every run
   - If scheduler runs every 10 minutes = **6 messages/hour** just for summaries

3. **No Rate Limiting**: No protection against message bursts

## Solution Implemented

### 1. Rate Limiting in `slack_utils.erl`

Added automatic rate limiting using ETS table:

- **Default**: 10 messages per minute
- **Configurable**: `SLACK_MAX_MESSAGES_PER_MINUTE` environment variable
- **Window-based**: Rolling 60-second window
- **Graceful degradation**: Messages exceeding limit are dropped with warning

**New functions**:
- `init_rate_limiter/0` - Initialize ETS table on start
- `check_rate_limit/0` - Check if message can be sent
- `get_max_messages_per_minute/0` - Get limit from env or default
- `reset_rate_limiter/0` - Manual reset (for testing)

### 2. Removed FSM Retry Spam

**Before** (`raptor_service_fsm.erl`):
```erlang
{error, Reason} ->
    send_slack_error(Name, Reason, Retry + 1),  % ❌ Sent on EVERY retry
    if Retry < ?MAX_RETRIES -> retry...
```

**After**:
```erlang
{error, Reason} ->
    if Retry < ?MAX_RETRIES -> 
        lager:warning("[~s] will retry...", [Name]),  % ✅ Only log
        retry...
    else ->
        send_slack_final_failure(...)  % ✅ Only on final failure
```

**Result**: Only **1 message per failed service** instead of **N messages per retry**

### 3. Configuration

Add to `raptor.env` or `.env`:

```bash
# Enable Slack notifications
export SLACK_NOTIFICATIONS_ENABLED=true

# Set rate limit (adjust based on your needs)
export SLACK_MAX_MESSAGES_PER_MINUTE=10

# Your webhook URL
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
```

### 4. Rate Limit Recommendations

Choose based on your traffic:

- **Development**: `SLACK_MAX_MESSAGES_PER_MINUTE=5` (low noise)
- **Production (low traffic)**: `SLACK_MAX_MESSAGES_PER_MINUTE=10` (default)
- **Production (high traffic)**: `SLACK_MAX_MESSAGES_PER_MINUTE=20` or batch messages

## Files Changed

1. ✅ `apps/common_lib/src/slack_utils.erl`
   - Added rate limiting with ETS table
   - Added `reset_rate_limiter/0` function
   - Added environment variable support

2. ✅ `apps/raptors/src/raptor_service_fsm.erl`
   - Removed `send_slack_error/3` function (unused now)
   - Removed retry notification call (line 105)
   - Kept only final failure notification

3. ✅ `devops/env/raptor.env`
   - Added `SLACK_MAX_MESSAGES_PER_MINUTE=10`

4. ✅ `apps/common_lib/SLACK_README.md`
   - Added rate limiting documentation
   - Added troubleshooting section for 429 errors
   - Added configuration examples

## Verification

### Check Rate Limiting in Action

```bash
# Search logs for rate-limited messages
grep "rate limited" log/console.log

# Expected output:
# [warning] Slack notification rate limited: ⚠️ Service X failed...
```

### Test Rate Limiter

In Erlang shell:

```erlang
%% Send 15 messages quickly
[slack_utils:notify_info(io_lib:format("Test ~p", [N])) || N <- lists:seq(1, 15)].

%% Expected: First 10 return 'ok', next 5 return '{error, rate_limited}'
```

### Monitor Slack API Responses

```bash
# Check for 429 errors in logs
grep "429" log/error.log
```

## Benefits

✅ **Prevents 429 errors**: Rate limiting stops message bursts  
✅ **Reduces noise**: No more retry spam in Slack channels  
✅ **Configurable**: Easy to adjust via environment variables  
✅ **Graceful**: Messages exceeding limit are logged, not lost silently  
✅ **Automatic**: No code changes needed in notification callers  

## Next Steps (Optional)

If you still see too many messages:

1. **Aggregate scheduler summaries**: Send one summary every N runs instead of every run
2. **Batch FSM results**: Collect all service results and send one summary
3. **Increase rate limit**: Set `SLACK_MAX_MESSAGES_PER_MINUTE=20` or higher
4. **Use Slack threads**: Group related messages in threads instead of separate messages

## Rollback (If Needed)

If rate limiting causes issues:

```bash
# Disable rate limiting by setting high limit
export SLACK_MAX_MESSAGES_PER_MINUTE=1000

# Or disable Slack entirely
export SLACK_NOTIFICATIONS_ENABLED=false
```

## Testing Checklist

- [ ] Build succeeds: `rebar3 compile`
- [ ] Rate limiter initializes on start
- [ ] Messages are rate-limited after threshold
- [ ] FSM only sends final failure, not retry messages
- [ ] Logs show "rate limited" warnings when limit exceeded
- [ ] No 429 errors in production logs after deployment
