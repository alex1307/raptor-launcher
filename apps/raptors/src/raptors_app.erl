-module(raptors_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting raptors supervisor..."),
    raptors_sup:start_link().

stop(_State) ->
    lager:info("Stopping raptors supervisor..."),
    ok.

