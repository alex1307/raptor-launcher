-module(chrome_utils_test).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------
%% integration tests for chrome_utils
%% ---------------------------------------------------------------

chrome_lifecycle_test_() ->
    {timeout, 30000, fun lifecycle/0}.

lifecycle() ->
   _Status = application:ensure_all_started(lager),
   
    %% 1️⃣ Проверка – Chrome не трябва да работи
    lager:info("Checking if Chrome is already running..."),
    IsRunning1 = chrome_utils:is_running(),
    ?assertEqual(false, IsRunning1),

    %% 2️⃣ Стартиране на Chrome
    lager:info("Starting Chrome..."),
    StartResult = chrome_utils:start(),
    ?assertMatch(ok, StartResult),

    timer:sleep(2000),

    %% 3️⃣ Проверка, че Chrome вече работи
    lager:info("Checking if Chrome is running after start..."),
    IsRunning2 = chrome_utils:chrome_info(),
    lager:info("Chrome info after start: ~p", [IsRunning2]),
    ?assertMatch({ok, _Map}, IsRunning2),

    %% 4️⃣ Спиране
    lager:info("Stopping Chrome."),
    StopResult = chrome_utils:stop(),

    ?assertMatch({ok, stopped}, StopResult),

    timer:sleep(1500),

    %% 5️⃣ Проверка, че е спрян
    lager:info("Checking if Chrome is stopped..."),
    Info = chrome_utils:chrome_info(),
    lager:info("Chrome info after stop: ~p", [Info]),
    ?assertMatch({error, failed_connect}, Info),
    lager:info("Chrome stopped successfully"),
    ok.   