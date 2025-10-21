%%%-------------------------------------------------------------------
%% @doc raptors top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(raptors_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define children under this supervisor
    RaptorsSrv = {
        raptors_srv,
        {raptors_srv, start_link, []},
        permanent,
        5000,
        worker,
        [raptors_srv]
    },

    {ok, {{one_for_one, 5, 10}, [RaptorsSrv]}}.