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
    supervisor_manager:start_child(chrome_manager_sup, chrome_fsm, start_link, []),
    supervisor_manager:start_child(mobile_de_manager_sup, mobile_de_fsm, start_link, []),

    % supervisor_manager:start_child(resource_manager_sup, resource_manager_fsm, start_link, []).
    
    {ok, self()}.

stop(_State) ->
    ok.

%% internal functions
