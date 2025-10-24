-module(raptors_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting raptors supervisor..."),
    Res = raptors_sup:start_link(),
    raptor_scheduler:schedule_services(raptor, ["raptor"]),
    raptor_scheduler:schedule_services(mobile_bg_crawler, ["crawler-bg"]),
    raptor_scheduler:schedule_services(autouncle_crawler, ["crawler-ro", 
        "crawler-fr", 
        "crawler-de", 
        "crawler-ch", 
        "crawler-it", 
        "crawler-pl", 
        "crawler-nl"]),
    Res.

stop(_State) ->
    lager:info("Stopping raptors supervisor..."),
    ok.

