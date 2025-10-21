-module(kafka_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define children under this supervisor
    KafkaSrv = {
        kafka_srv,
        {kafka_srv, start_link, []},
        permanent,
        5000,
        worker,
        [kafka_srv]
    },

    {ok, {{one_for_one, 5, 10}, [KafkaSrv]}}.