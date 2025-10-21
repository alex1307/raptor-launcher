-module(kafka_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting kafka supervisor..."),
    kafka_sup:start_link().

stop(_State) ->
    lager:info("Stopping kafka supervisor..."),
    ok.
