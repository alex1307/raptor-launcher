-module(network_utils).
-export([check_all/0]).

-spec(check_all() -> ok | {error, atom()}).
check_all() ->
    case check_internet() of
        ok ->
            case check_docker_network() of
                ok -> ok;
                Err -> Err
            end;
        Err -> Err
    end.

check_internet() ->
    case os:cmd("ping -c 1 8.8.8.8 > /dev/null 2>&1 && echo ok || echo fail") of
        "ok\n" -> ok;
        _ -> {error, no_internet}
    end.

check_docker_network() ->
    case os:cmd("docker network ls --format '{{.Name}}' | grep -q raptor_my_network && echo ok || echo fail") of
        "ok\n" -> ok;
        _ -> {error, docker_network_missing}
    end.