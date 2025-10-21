-module(raptor_utils_test).
-include_lib("eunit/include/eunit.hrl").


%% ---------------------------------------------------------------
%% Unit tests for raptor_utils
%% ---------------------------------------------------------------  

raptor_test_() ->
    {timeout, 10000, fun is_running/0}.

is_running() ->
    _Status = application:ensure_all_started(lager),
    Config = raptor_utils:check_config(),
    ?assertEqual(ok, Config),
    lager:info("Starting is_running_test..."),
    ?assertEqual(false, raptor_utils:is_running(mobile_de, "")),
    lager:info("mobile_de is_running test passed."),
    ?assertEqual(false, raptor_utils:is_running(crawler, "")),
    lager:info("crawler is_running test passed."),
    ?assertEqual(false, raptor_utils:is_running(raptor, "")),
    lager:info("raptor is_running test passed."),
    ?assertEqual(not_implemented, raptor_utils:is_running(unknown_service, "")),
    lager:info("unknown_service is_running test passed."),
    lager:info("is_running_test completed successfully."),
    ok.

%---------------------------------------------------------------
%% Integration tests for raptor_utils
%%% --------------------------------------------------------------- 
raptor_lifecycle_test_() ->
    {timeout, 60000, fun lifecycle/0}.
lifecycle() ->
   _Status = application:ensure_all_started(lager),
   Config = raptor_utils:check_config(),
   ?assertEqual(ok, Config),
   lager:info("Starting raptor lifecycle test..."),

    %% 1️⃣ Проверка – услугите не трябва да работят
   ?assertEqual(false, raptor_utils:is_running(mobile_de, "")),
   ?assertEqual(false, raptor_utils:is_running(crawler, "")),
   ?assertEqual(false, raptor_utils:is_running(raptor, "")),
   
    %% 2️⃣ Стартиране на услугите
    lager:info("Starting mobile_de..."),
    StartResult1 = raptor_utils:start(mobile_de, []),
    ?assertMatch({ok, started}, StartResult1),
    timer:sleep(2000),
    lager:info("Starting crawler..."),
    StartResult2 = raptor_utils:start(crawler, ""),
    ?assertMatch({ok, started}, StartResult2),
    timer:sleep(2000),
    lager:info("Starting raptor..."),
    StartResult3 = raptor_utils:start(raptor, ""),
    ?assertMatch({ok, started}, StartResult3),
    timer:sleep(3000),
    %% 3️⃣ Проверка, че услугите вече работят
    ?assertEqual(true, raptor_utils:is_running(mobile_de, "")),
    ?assertEqual(true, raptor_utils:is_running(crawler, "")),
    ?assertEqual(true, raptor_utils:is_running(raptor, "")), 
    %% 4️⃣ Спиране на услугите
    lager:info("Stopping raptor..."),
    StopResult1 = raptor_utils:stop(raptor),
    ?assertMatch({ok, stopped}, StopResult1),
    timer:sleep(2000),
    lager:info("Stopping crawler..."),
    timer:sleep(2000),
    lager:info("Stopping mobile_de..."),
    StopResult3 = raptor_utils:stop(mobile_de),
    ?assertMatch({ok, stopped}, StopResult3),
    timer:sleep(2000),
    %% 5️⃣ Проверка, че услугите са спрени
    ?assertEqual(false, raptor_utils:is_running(mobile_de, "")),
    ?assertEqual(false, raptor_utils:is_running(crawler, "")),
    ?assertEqual(false, raptor_utils:is_running(raptor, "")),
    lager:info("Raptor lifecycle test completed successfully."),

   ok.