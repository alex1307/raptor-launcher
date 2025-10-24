-module(raptors_utils).
-export([
    list_services/1,
    is_service_running/2,
    start_service/2,
    stop_service/2,
    status/1
]).


%% ----------------------------------------------------------------------------
%% Raptor Services Utilities - YAML driven commands
%% ----------------------------------------------------------------------------

%% List all defined services
-spec list_services(map()) -> [string()].
list_services(ConfigMap) ->
    maps:keys(ConfigMap).

%% ----------------------------------------------------------------------------
%% Check if a specific service is running
%% ----------------------------------------------------------------------------
-spec is_service_running(map(), string()) -> boolean().
is_service_running(ConfigMap, ServiceName) ->
    case maps:get(ServiceName, ConfigMap, undefined) of
        undefined ->
            lager:error("Service ~p not found in configuration", [ServiceName]),
            false;
        Service ->
            Cmd = maps:get("grep_cmd", Service),
            lager:debug("Checking if Raptor service ~p is running with command: ~s", [ServiceName, Cmd]),
            case cmd_utils:execute(Cmd) of
                {ok, Output} ->
                    lager:debug("Command output: ~s", [Output]),
                    string:find(Output,"not found") == nomatch;
                {error, _Reason} ->
                    false
            end
            
    end.

%% ----------------------------------------------------------------------------
%% Start a specific Raptor service
%% ----------------------------------------------------------------------------
-spec start_service(map(), string()) -> {ok, string()} | {error, string()}.
start_service(ConfigMap, ServiceName) ->
    case maps:get(ServiceName, ConfigMap, undefined) of
        undefined ->
            {error, "service_not_found"};
        Service ->
            case maps:get("enabled", Service, false) of
                false ->
                    lager:warning("Service ~p is disabled.", [ServiceName]),
                    {error, "disabled"};
                true ->
                    StartScript = maps:get("start_script", Service),
                    lager:debug("Starting Raptor service ~p: ~s", [ServiceName, StartScript]),
                    cmd_utils:execute("bash " ++ StartScript ++ " >> /dev/null 2>&1 &")
            end
    end.

%% ----------------------------------------------------------------------------
%% Stop a specific Raptor service
%% ----------------------------------------------------------------------------
-spec stop_service(map(), string()) -> {ok, string()} | {error, string()}.
stop_service(ConfigMap, ServiceName) ->
    case maps:get(ServiceName, ConfigMap, undefined) of
        undefined ->
            {error, "service_not_found"};
        Service ->
            StopScript = maps:get("stop_script", Service),
            lager:debug("Stopping Raptor service ~p: ~s", [ServiceName, StopScript]),
            cmd_utils:execute("bash " ++ StopScript)
    end.

%% ----------------------------------------------------------------------------
%% Return overall status map
%% ----------------------------------------------------------------------------
-spec status(map()) -> {ok, map()}.
status(ConfigMap) ->
    Results =
        [begin
             Running = is_service_running(ConfigMap, Name),
             {Name, Running}
         end || Name <- maps:keys(ConfigMap)],
    {ok, maps:from_list(Results)}.