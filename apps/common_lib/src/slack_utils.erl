%%% ====================================================================
%%% Slack notification service
%%% Provides functionality to send messages to Slack via webhook
%%% ====================================================================

-module(slack_utils).
-export([
    notify/1,
    notify_info/1,
    notify_warning/1,
    notify_error/1,
    notify_success/1,
    is_enabled/0,
    start/0,
    stop/0,
    reset_rate_limiter/0
]).

-define(DEFAULT_TIMEOUT, 5000).
-define(SLACK_API_URL_ENV, "SLACK_WEBHOOK_URL").
-define(SLACK_ENABLED_ENV, "SLACK_NOTIFICATIONS_ENABLED").
-define(SLACK_RATE_LIMIT_ENV, "SLACK_MAX_MESSAGES_PER_MINUTE").

%% Rate limiting settings
-define(DEFAULT_MAX_MESSAGES_PER_MINUTE, 10).  % Default max 10 messages per minute
-define(RATE_LIMIT_WINDOW_MS, 60000).  % 1 minute window
-define(RATE_LIMITER_TABLE, slack_rate_limiter).

%% Message types with emoji prefixes
-define(INFO_PREFIX, "ℹ️").
-define(WARNING_PREFIX, "⚠️").
-define(ERROR_PREFIX, "❌").
-define(SUCCESS_PREFIX, "✅").

%%% ====================== API =========================================

% Start the HTTP client and rate limiter
start() ->
    application:ensure_all_started(inets),
    init_rate_limiter().

% Stop the HTTP client  
stop() ->
    cleanup_rate_limiter(),
    application:stop(inets).

% Reset rate limiter (for testing or manual reset)
-spec reset_rate_limiter() -> ok.
reset_rate_limiter() ->
    cleanup_rate_limiter(),
    init_rate_limiter().

% Check if Slack notifications are enabled
-spec is_enabled() -> boolean().
is_enabled() ->
    case os:getenv(?SLACK_ENABLED_ENV) of
        "true" -> true;
        "1" -> true;
        _ -> false
    end.

% Send a simple text message
-spec notify(iodata() | string()) -> ok | {error, term()}.
notify(Message) ->
    case is_enabled() of
        false -> 
            lager:debug("Slack notifications disabled, message: ~s", [Message]),
            ok;
        true ->
            case check_rate_limit() of
                ok ->
                    send_message(Message);
                {error, rate_limited} ->
                    lager:warning("Slack notification rate limited: ~s", [Message]),
                    {error, rate_limited}
            end
    end.

% Send info message with emoji
-spec notify_info(iodata() | string()) -> ok | {error, term()}.
notify_info(Message) ->
    notify([?INFO_PREFIX, " ", Message]).

% Send warning message with emoji
-spec notify_warning(iodata() | string()) -> ok | {error, term()}.
notify_warning(Message) ->
    notify([?WARNING_PREFIX, " ", Message]).

% Send error message with emoji
-spec notify_error(iodata() | string()) -> ok | {error, term()}.
notify_error(Message) ->
    notify([?ERROR_PREFIX, " ", Message]).

% Send success message with emoji
-spec notify_success(iodata() | string()) -> ok | {error, term()}.
notify_success(Message) ->
    notify([?SUCCESS_PREFIX, " ", Message]).

%%% ====================== Internal ===================================

% Get Slack webhook URL from environment
-spec get_webhook_url() -> {ok, string()} | {error, not_configured}.
get_webhook_url() ->
    case os:getenv(?SLACK_API_URL_ENV) of
        false -> {error, not_configured};
        Url when is_list(Url) -> {ok, Url};
        _ -> {error, not_configured}
    end.

% Build JSON payload for Slack
-spec build_json_payload(iodata() | string()) -> binary().
build_json_payload(Message) when is_list(Message) ->
    %% Escape special JSON characters
    EscapedText = escape_json_string(Message),
    %% Convert Unicode string to UTF-8 binary
    TextBin = unicode:characters_to_binary(EscapedText, utf8),
    %% Build JSON as binary
    <<"{\"text\":\"", TextBin/binary, "\"}">>;
build_json_payload(Message) ->
    %% Convert iodata to string first
    Text = lists:flatten(io_lib:format("~ts", [Message])),
    build_json_payload(Text).

% Escape JSON string
-spec escape_json_string(string()) -> string().
escape_json_string(Str) ->
    escape_json_chars(Str, []).

escape_json_chars([], Acc) ->
    lists:reverse(Acc);
escape_json_chars([$" | Rest], Acc) ->
    escape_json_chars(Rest, [$", $\\ | Acc]);
escape_json_chars([$\\ | Rest], Acc) ->
    escape_json_chars(Rest, [$\\, $\\ | Acc]);
escape_json_chars([$\n | Rest], Acc) ->
    escape_json_chars(Rest, [$n, $\\ | Acc]);
escape_json_chars([$\r | Rest], Acc) ->
    escape_json_chars(Rest, [$r, $\\ | Acc]);
escape_json_chars([$\t | Rest], Acc) ->
    escape_json_chars(Rest, [$t, $\\ | Acc]);
escape_json_chars([C | Rest], Acc) ->
    escape_json_chars(Rest, [C | Acc]).

% Send message to Slack
-spec send_message(iodata() | string()) -> ok | {error, term()}.
send_message(Message) ->
    case get_webhook_url() of
        {error, not_configured} ->
            lager:warning("Slack webhook URL not configured"),
            {error, not_configured};
        {ok, Url} ->
            Payload = build_json_payload(Message),
            send_http_request(Url, Payload)
    end.

% Send HTTP request to Slack
-spec send_http_request(string(), binary()) -> ok | {error, term()}.
send_http_request(Url, Payload) ->
    % Ensure inets is started for httpc
    case application:ensure_all_started(inets) of
        {ok, _} -> 
            do_send_http_request(Url, Payload);
        {error, Reason} ->
            lager:warning("Could not start inets: ~p", [Reason]),
            {error, {inets_not_available, Reason}}
    end.

-spec do_send_http_request(string(), binary()) -> ok | {error, term()}.
do_send_http_request(Url, Payload) ->
    Headers = [{"Content-Type", "application/json; charset=utf-8"}],
    HTTPOptions = [{timeout, ?DEFAULT_TIMEOUT}],
    RequestOptions = [],
    
    case httpc:request(post, {Url, Headers, "application/json", Payload}, 
                      HTTPOptions, RequestOptions) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} ->
            lager:debug("Slack message sent successfully"),
            ok;
        {ok, {{_Version, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            lager:error("Slack API error: ~p ~s, Body: ~s", [StatusCode, ReasonPhrase, Body]),
            {error, {http_error, StatusCode, ReasonPhrase}};
        {error, Reason} ->
            lager:error("HTTP request to Slack failed: ~p", [Reason]),
            {error, {http_request_failed, Reason}}
    end.

%%% ====================== Rate Limiter ===============================

%% Get max messages per minute from environment or use default
-spec get_max_messages_per_minute() -> pos_integer().
get_max_messages_per_minute() ->
    case os:getenv(?SLACK_RATE_LIMIT_ENV) of
        false -> ?DEFAULT_MAX_MESSAGES_PER_MINUTE;
        Str ->
            case string:to_integer(Str) of
                {Int, ""} when Int > 0 -> Int;
                _ -> ?DEFAULT_MAX_MESSAGES_PER_MINUTE
            end
    end.

%% Initialize rate limiter ETS table
-spec init_rate_limiter() -> ok.
init_rate_limiter() ->
    case ets:info(?RATE_LIMITER_TABLE) of
        undefined ->
            ets:new(?RATE_LIMITER_TABLE, [named_table, public, set]),
            ets:insert(?RATE_LIMITER_TABLE, {message_count, 0}),
            ets:insert(?RATE_LIMITER_TABLE, {window_start, erlang:monotonic_time(millisecond)}),
            ok;
        _ ->
            ok
    end.

%% Cleanup rate limiter ETS table
-spec cleanup_rate_limiter() -> ok.
cleanup_rate_limiter() ->
    case ets:info(?RATE_LIMITER_TABLE) of
        undefined -> ok;
        _ -> 
            ets:delete(?RATE_LIMITER_TABLE),
            ok
    end.

%% Check if we can send a message (rate limiting)
-spec check_rate_limit() -> ok | {error, rate_limited}.
check_rate_limit() ->
    Now = erlang:monotonic_time(millisecond),
    MaxMessages = get_max_messages_per_minute(),
    
    case ets:lookup(?RATE_LIMITER_TABLE, window_start) of
        [{window_start, WindowStart}] ->
            TimeSinceWindowStart = Now - WindowStart,
            
            if
                TimeSinceWindowStart > ?RATE_LIMIT_WINDOW_MS ->
                    %% New window - reset counter
                    ets:insert(?RATE_LIMITER_TABLE, {message_count, 1}),
                    ets:insert(?RATE_LIMITER_TABLE, {window_start, Now}),
                    ok;
                true ->
                    %% Still in same window - check count
                    [{message_count, Count}] = ets:lookup(?RATE_LIMITER_TABLE, message_count),
                    
                    if
                        Count < MaxMessages ->
                            ets:insert(?RATE_LIMITER_TABLE, {message_count, Count + 1}),
                            ok;
                        true ->
                            {error, rate_limited}
                    end
            end;
        [] ->
            %% No window - initialize
            init_rate_limiter(),
            ok
    end.



