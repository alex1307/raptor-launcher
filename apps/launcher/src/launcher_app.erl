%%%-------------------------------------------------------------------
%% @doc launcher public API
%% @end
%%%-------------------------------------------------------------------

-module(launcher_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    supervisor_manager:start_link(),
    supervisor_manager:start_child(docker_manager_sup, docker_fsm, start_link, []),
    supervisor_manager:start_child(chrome_manager_sup, chrome_fsm, start_link, []).
    % supervisor_manager:start_child(resource_manager_sup, resource_manager_fsm, start_link, []).

stop(_State) ->
    ok.

%% internal functions
