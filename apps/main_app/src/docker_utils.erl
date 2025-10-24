-module(docker_utils).
-export([
    is_docker_alive/0,
    is_container_running/1,
    status/0
]).
-define(KAFKA_CONTAINER, 'kafka-server').
-define(POSTGRES_CONTAINER, 'postgres-server').
-define(WEB_ADMIN_CONTAINER, 'web-admin').


%% Проверка дали Docker daemon работи
-spec is_docker_alive() -> boolean().
is_docker_alive() ->
    case cmd_utils:execute("docker info > /dev/null 2>&1 && echo ok || echo error") of
        {ok, _} -> true;
        _ -> 
            lager:debug("Docker daemon is not alive"),
            false
    end.


%% Проверка дали даден контейнер е стартиран
-spec is_container_running(Name :: atom()) -> boolean().
is_container_running(Name) ->
    Cmd = "docker inspect -f '{{.State.Running}}' " ++ atom_to_list(Name),
    case cmd_utils:execute(Cmd) of
        {ok, Output} -> 
            string:trim(Output) =:= "true";
        _ -> 
            false
    end.

%% Обобщен статус на docker и ключовите услуги
-spec status() -> {ok, list()} | {error, string()}.
status() ->
    case is_docker_alive() of
        false -> {error, "docker not running"};
        true ->
            Services = [?POSTGRES_CONTAINER, ?KAFKA_CONTAINER, ?WEB_ADMIN_CONTAINER],
            Results = [{S, is_container_running(S)} || S <- Services],
            {ok, Results}
    end.

