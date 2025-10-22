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
    stop/0
]).

-define(DEFAULT_TIMEOUT, 5000).
-define(SLACK_API_URL_ENV, "SLACK_WEBHOOK_URL").
-define(SLACK_ENABLED_ENV, "SLACK_NOTIFICATIONS_ENABLED").

%% Message types with emoji prefixes
-define(INFO_PREFIX, "ℹ️").
-define(WARNING_PREFIX, "⚠️").
-define(ERROR_PREFIX, "❌").
-define(SUCCESS_PREFIX, "✅").

%%% ====================== API =========================================

% Start the HTTP client (if needed)
start() ->
    application:ensure_all_started(inets).

% Stop the HTTP client  
stop() ->
    application:stop(inets).

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
            send_message(Message)
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


