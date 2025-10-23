%%% ------------------------------------------------------------------
%%% MOBILE_DE FSM
%%% Manages mobile_de service lifecycle
%%% States represent actual system conditions:
%%%   wait_for_kafka -> check_mobile_de -> ready -> monitoring
%%% ------------------------------------------------------------------

-module(mobile_de_fsm).
-behaviour(gen_statem).

-export([start_link/0, stop/0, kafka_ready/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% state functions
-export([
    wait_for_kafka/3,
    check_mobile_de/3,
    ready/3,
    monitoring/3
]).

-define(MONITOR_MS, 30000).
-define(RETRY_MS, 5000).
-define(MAX_RETRIES, 10).

-record(state, {
    retries = 0 :: non_neg_integer()
}).

%%% ====================== API =========================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

%% Called by docker_fsm when Kafka is ready
kafka_ready() ->
    gen_statem:cast(?MODULE, kafka_ready).

%%% ====================== gen_statem ==================================

callback_mode() -> [state_functions, state_enter].

init([]) ->
    lager:info("mobile_de_fsm: initializing, waiting for Kafka"),
    {ok, wait_for_kafka, #state{}}.

terminate(_Reason, _StateName, _Data) ->
    lager:info("mobile_de_fsm: terminated"),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%% ====================== Helper Functions ============================

%% Slack notification helpers (safe wrappers)
slack_safe_info(Msg) ->
    try
        slack_utils:notify_info(Msg)
    catch
        _:_ -> ok
    end.

slack_safe_error(Msg) ->
    try
        slack_utils:notify_error(Msg)
    catch
        _:_ -> ok
    end.

%%% ====================== State Functions =============================

%% Wait for Kafka to be configured before starting mobile_de
wait_for_kafka(enter, _OldState, S) ->
    lager:info("mobile_de_fsm: waiting for Kafka to be configured"),
    slack_safe_info("⏳ Waiting for Kafka configuration..."),
    {keep_state, S};
wait_for_kafka(cast, kafka_ready, S) ->
    lager:info("mobile_de_fsm: Kafka ready, proceeding to check mobile_de"),
    slack_safe_info("✅ Kafka ready, starting mobile_de checks"),
    {next_state, check_mobile_de, S};
wait_for_kafka(_Type, _Event, S) ->
    {keep_state, S}.

%% Check if mobile_de is running
check_mobile_de(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_mobile_de(state_timeout, check, S) ->
    lager:info("mobile_de_fsm: checking if mobile_de is running"),
    slack_safe_info("🌐 Checking mobile_de status..."),
    case raptors_srv:is_service_running("mobile_de") of
        true ->
            lager:info("mobile_de_fsm: mobile_de already running"),
            slack_safe_info("✅ mobile_de is running"),
            {next_state, ready, S#state{retries = 0}};
        false ->
            lager:info("mobile_de_fsm: mobile_de not running, attempting to start"),
            slack_safe_info("🌐 Starting mobile_de..."),
            case raptors_srv:start_service("mobile_de") of
                {ok, _} ->
                    slack_safe_info("✅ mobile_de started successfully"),
                    {next_state, ready, S#state{retries = 0}};
                {error, Reason} ->
                    lager:error("mobile_de_fsm: failed to start mobile_de: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ mobile_de failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    retry_or_fail(check_mobile_de, S)
            end
    end;
check_mobile_de(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check_mobile_de}]}.

%% mobile_de is ready, start monitoring
ready(enter, _OldState, S) ->
    lager:info("mobile_de_fsm: mobile_de ready, starting monitoring"),
    slack_safe_info("🎉 mobile_de    is ready and monitoring!"),
    {keep_state, S, [{state_timeout, 0, start_monitoring}]};
ready(state_timeout, start_monitoring, S) ->
    {next_state, monitoring, S, [{state_timeout, ?MONITOR_MS, monitor}]};
ready({call, From}, get_status, S) ->
    gen_statem:reply(From, {ready, S}),
    {keep_state, S};
ready(_Type, _Event, S) ->
    {keep_state, S}.

%% Continuously monitor mobile_de health
monitoring(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
monitoring(state_timeout, monitor, S) ->
    case raptors_srv:is_service_running("mobile_de") of
        true ->
            {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
        false ->
            lager:warning("mobile_de_fsm: mobile_de down during monitoring, restarting check"),
            {next_state, check_mobile_de, S#state{retries = 0}}
    end;
monitoring({call, From}, get_status, S) ->
    gen_statem:reply(From, {monitoring, S}),
    {keep_state, S};
monitoring(_Type, _Event, S) ->
    {keep_state, S}.

%%% ====================== Helper Functions ============================

retry_or_fail(State, S = #state{retries = N}) when N < ?MAX_RETRIES ->
    lager:warning("mobile_de_fsm: retry ~p/~p for state ~p", [N+1, ?MAX_RETRIES, State]),
    {keep_state, S#state{retries = N+1}, [{state_timeout, ?RETRY_MS, retry}]};
retry_or_fail(State, S) ->
    lager:error("mobile_de_fsm: max retries reached in state ~p, staying in state", [State]),
    {keep_state, S, [{state_timeout, ?RETRY_MS * 2, retry}]}.
