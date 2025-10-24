-module(docker_utils_yml).
-export([
    is_docker_alive/1,
    is_container_running/2,
    status/1,
    start_container/2,
    stop_container/2,
    restart_container/2,
    start_compose/1,
    stop_compose/1,
    restart_compose/1,
    start_docker_daemon/1,
    stop_docker_daemon/1
]).

%% ----------------------------------------------------------------------------



%% Check Docker daemon
-spec is_docker_alive(maps:map()) -> boolean().
is_docker_alive(ConfigMap) ->
    Cmd = maps:get("is_alive", maps:get("commands", ConfigMap)),
    case os:cmd(Cmd) of
        "ok\n" -> true;
        _ -> false
    end.

%% Check if container is running
-spec is_container_running(maps:map(), string()) -> boolean().
is_container_running(ConfigMap, Name) ->
    Commands = maps:get("commands", ConfigMap),
    CmdTemplate = maps:get("is_container_running", Commands),
    Cmd = string:replace(CmdTemplate, "{{container}}", Name, all),
    case os:cmd(Cmd) of
        "true\n" -> true;
        _ -> false
    end.

%% Return summary status of Docker and containers
-spec status(maps:map()) -> {ok, list()} | {error, string()}.
status(ConfigMap) ->
    case is_docker_alive(ConfigMap) of
        false ->
            lager:error("Docker daemon is not running."),
            {error, "docker not running"};
        true ->
            Containers = maps:get("containers", ConfigMap),
            Results = [{C, is_container_running(ConfigMap, C)} || C <- Containers],
            {ok, Results}
    end.  


-spec exec_container_cmd(maps:map(), string(), string()) -> {ok, string()} | {error, string()}.
exec_container_cmd(ConfigMap, Container, Key) ->

    CmdTemplate = maps:get(Key, maps:get("commands", ConfigMap)),
    WorkDir = maps:get("compose_workdir", ConfigMap),
    Cmd1 = string:replace(CmdTemplate, "{{container}}", Container, all),
    CmdFinal = string:replace(Cmd1, "{{workdir}}", WorkDir, all),
    cmd_utils:execute(CmdFinal).

-spec exec_compose_cmd(maps:map(), string()) -> {ok, string()} | {error, string()}.
exec_compose_cmd(ConfigMap, Key) ->
    WorkDir = maps:get("compose_workdir", ConfigMap),
    CmdTemplate = maps:get(Key, maps:get("commands", ConfigMap)),
    Cmd = string:replace(CmdTemplate, "{{workdir}}", WorkDir, all),
    cmd_utils:execute(Cmd).

-spec start_container(maps:map(), string()) -> {ok, string()} | {error, string()}.
start_container(ConfigMap, Container) ->
    exec_container_cmd(ConfigMap, Container, "start_container").

-spec stop_container(maps:map(), string()) -> {ok, string()} | {error, string()}.
stop_container(ConfigMap, Container) ->
    exec_container_cmd(ConfigMap, Container, "stop_container").

-spec restart_container(maps:map(), string()) -> {ok, string()} | {error, string()}.
restart_container(ConfigMap, Container) ->
    exec_container_cmd(ConfigMap, Container, "restart_container").

-spec start_compose(maps:map()) -> {ok, string()} | {error, string()}.
start_compose(ConfigMap) ->
    exec_compose_cmd(ConfigMap, "start_compose").

-spec stop_compose(maps:map()) -> {ok, string()} | {error, string()}.
stop_compose(ConfigMap) ->
    exec_compose_cmd(ConfigMap, "stop_compose").

-spec restart_compose(maps:map()) -> {ok, string()} | {error, string()}.
restart_compose(ConfigMap) ->
    exec_compose_cmd(ConfigMap, "restart_compose").

%% ----------------------------------------------------------------------------
-spec start_docker_daemon(maps:map()) -> {ok, string()} | {error, string()}.
start_docker_daemon(ConfigMap) ->
    StartDockerKey = case os:type() of
        {unix, linux} -> "start_docker_linux";
        {unix, darwin} -> "start_docker_mac";
        _ -> "start_docker_daemon_unknown"
    end,
    Cmd = maps:get(StartDockerKey, maps:get("commands", ConfigMap)),
    cmd_utils:execute(Cmd).

%%--------------------------------------------------------------------
%% @doc Stop Docker daemon
%%--------------------------------------------------------------------
-spec stop_docker_daemon(maps:map()) -> {ok, string()} | {error, string()}.
stop_docker_daemon(ConfigMap) ->
    StopDockerKey = case os:type() of
        {unix, linux} -> "stop_docker_linux";
        {unix, darwin} -> "stop_docker_mac";
        _ -> "stop_docker_daemon_unknown"
    end,
    Cmd = maps:get(StopDockerKey, maps:get("commands", ConfigMap)),
    cmd_utils:execute(Cmd).
