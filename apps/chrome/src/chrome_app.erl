-module(chrome_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting chrome supervisor..."),
    chrome_sup:start_link().

stop(_State) ->
    lager:info("Stopping chrome supervisor..."),
    ok.
