-module(chrome_utils).
-export([
    is_chrome_running/1,
    start_chrome/1,
    stop_chrome/1,
    status/1
]).


%% ----------------------------------------------------------------------------
%% Chrome utilities - YAML driven commands
%% ----------------------------------------------------------------------------

%% Проверява дали Chrome процесът е активен (grep/pgrep команда)
-spec is_chrome_running(map()) -> boolean().
is_chrome_running(ConfigMap) ->
    Port = maps:get("port", ConfigMap),
    Cmd = maps:get("grep_cmd", ConfigMap),
    ActualCmd = string:replace(Cmd, "{{port}}", integer_to_list(Port)),
    lager:info("Checking Chrome process: ~s", [ActualCmd]),
    case os:cmd(ActualCmd) of
        "not found\n" -> false;
        Output ->
            lager:info("Chrome grep result: ~s", [string:trim(Output)]),
            not lists:member("not found", string:lowercase(Output))
    end.

%% Старт на Chrome
-spec start_chrome(map()) -> {ok, string()} | {error, string()}.
start_chrome(ConfigMap) ->
    StartScript = maps:get("start_cmd", ConfigMap),
    Cmd = io_lib:format("bash ~s >> /dev/null 2>&1 &", [StartScript]),
    lager:info("Starting Chrome: ~s", [lists:flatten(Cmd)]),
    cmd_utils:execute(lists:flatten(Cmd)).

%% Спира Chrome
-spec stop_chrome(map()) -> {ok, string()} | {error, string()}.
stop_chrome(ConfigMap) ->
    Cmd = maps:get("stop_cmd", ConfigMap),
    Port = maps:get("port", ConfigMap),
    ActualCmd = string:replace(Cmd, "{{port}}", integer_to_list(Port)),
    lager:info("Stopping Chrome: ~s", [ActualCmd]),
    cmd_utils:execute(ActualCmd).

%% Обобщен статус
-spec status(map()) -> {ok, map()} | {error, string()}.
status(ConfigMap) ->
    case is_chrome_running(ConfigMap) of
        true ->
            Port = maps:get("port", ConfigMap),
            VersionUrl = maps:get("version_url", ConfigMap),
            lager:info("Chrome detected on port ~p, checking version...", [Port]),
            Url = string:replace(VersionUrl, "{{port}}", integer_to_list(Port)),
            case httpc:request(get, {Url, []}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    {ok, #{running => true, version => iolist_to_binary(Body)}};
                _ ->
                    {ok, #{running => true, version => "unknown"}}
            end;
        false ->
            lager:warning("Chrome is not running."),
            {ok, #{running => false}}
    end.