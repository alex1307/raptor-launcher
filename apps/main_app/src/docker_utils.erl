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
    case os:cmd("docker info > /dev/null 2>&1 && echo ok || echo error") of
        "ok\n" -> true;
        Res -> 
            lager:debug("Response ~p", [Res]),
            false
    end.


%% Проверка дали даден контейнер е стартиран
-spec is_container_running(Name :: atom()) -> boolean().
is_container_running(Name) ->
    Cmd = "docker inspect -f '{{.State.Running}}' " ++ Name,
    case os:cmd(Cmd) of
        "true\n" -> true;
        "false\n" -> false;
        _ -> false
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

