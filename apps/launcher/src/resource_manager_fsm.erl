%%% ====================================================================
%%%  Resource Orchestrator FSM
%%%  Ensures infra dependencies are up:
%%%  Network → Docker → PostgreSQL → Kafka → Chrome
%%%
%%%  Features:
%%%   - Boot pipeline with retries
%%%   - Periodic monitoring (health loop)
%%%   - Automatic recovery (restart containers / chrome)
%%%   - Slack notifications on each step
%%% ====================================================================

-module(resource_manager_fsm).
-behaviour(gen_statem).


%% API
-export([start_link/0, status/0]).

%% gen_statem
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% state callbacks (state_functions mode)
-export([
    starting_network/3,
    starting_docker/3,
    starting_postgres/3,
    starting_kafka/3,
    starting_chrome/3,
    ready/3,
    recovering_kafka/3,
    recovering_postgres/3,
    recovering_docker/3,
    recovering_chrome/3
]).

-define(MONITOR_MS, 30000).     %% health check interval
-define(RETRY_MS,    5000).     %% base retry delay
-define(MAX_BACKOFF, 60000).    %% 1 min cap

-record(state, {
    %% readiness flags
    network_ok  = false :: boolean(),
    docker_ok   = false :: boolean(),
    postgres_ok = false :: boolean(),
    kafka_ok    = false :: boolean(),
    chrome_ok   = false :: boolean(),
    ready       = false :: boolean(),

    %% retry counters for exponential backoff
    retries = #{
        docker   => 0,
        postgres => 0,
        kafka    => 0,
        chrome   => 0,
        network  => 0
    } :: map()
}).

%%% ====================== API =========================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_statem:call(?MODULE, get_status).

%%% ====================== gen_statem ==================================

callback_mode() -> state_functions.

init([]) ->
    lager:info("🔧 Resource Orchestrator initializing..."),
    slack_safe_info("🔧 Resource Orchestrator starting..."),
    {ok, starting_network, #state{}, [{next_event, internal, start_network}]}.

terminate(_Reason, _State, _Data) ->
    lager:info("Resource Orchestrator stopped."),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%% ====================== Helpers =====================================

next_backoff(Key, S = #state{retries = R0}) ->
    N = maps:get(Key, R0, 0),
    R = R0#{Key => N+1},
    %% exponential backoff: RETRY_MS * 2^N, capped to MAX_BACKOFF
    Delay = erlang:min(?MAX_BACKOFF, trunc(?RETRY_MS * math:pow(2, N))),
    {Delay, S#state{retries = R}}.

reset_backoff(Key, S = #state{retries = R0}) ->
    S#state{retries = R0#{Key => 0}}.



%% Slack wrappers (don't crash if slack utils/env missing)
-spec slack_safe_info(string() | iodata()) -> ok.
-spec slack_safe_error(string() | iodata()) -> ok.

slack_safe_info(Msg) when is_list(Msg) ->
    %% If it's already a string, use it directly
    lager:info("Slack: ~ts", [Msg]),
    try
        slack_utils:notify_info(Msg)
    catch
        _:Error ->
            lager:warning("Failed to send Slack notification: ~p", [Error]),
            ok
    end;
slack_safe_info(Msg) ->
    %% If it's iodata, flatten it first
    MsgStr = lists:flatten(io_lib:format("~ts", [Msg])),
    slack_safe_info(MsgStr).
    
slack_safe_error(Msg) when is_list(Msg) ->
    %% If it's already a string, use it directly
    lager:error("Slack: ~ts", [Msg]),
    try
        slack_utils:notify_error(Msg)
    catch
        _:Error ->
            lager:warning("Failed to send Slack notification: ~p", [Error]),
            ok
    end;
slack_safe_error(Msg) ->
    %% If it's iodata, flatten it first
    MsgStr = lists:flatten(io_lib:format("~ts", [Msg])),
    slack_safe_error(MsgStr).

%% ------------ Health checks (delegate to your utils) ------------------

-spec health_network() -> ok | {error, term()}.
health_network() ->
    case network_utils:check_all() of
        ok -> ok;
        Err -> Err
    end.

-spec health_docker() -> ok | {error, term()}.
health_docker() ->
    %% docker alive check from YAML commands (docker_utils_yml:is_alive/0 expected)
    case docker_srv:is_docker_running() of
        true  -> ok;
        false -> {error, docker_down}
    end.

-spec health_postgres() -> ok | {error, term()}.
health_postgres() ->
    case docker_srv:is_container_running("postgres-server") of
        true  -> ok;
        false -> {error, postgres_down}
    end.

-spec health_kafka() -> ok | {error, term()}.
health_kafka() ->
    case docker_srv:is_container_running("kafka-server") of
        true  -> ok;
        false -> {error, kafka_down}
    end.

-spec health_chrome() -> ok | {error, term()}.
health_chrome() ->
    case chrome_srv:is_chrome_running() of
        true  -> ok;
        false -> {error, chrome_down}
    end.

%% ------------ Composite health used by monitor loop -------------------

-spec check_health(#state{}) ->
          ok
        | {error, down, docker | postgres | kafka | chrome | network}.
check_health(_S) ->
    HealthChecks = [
        {network, fun health_network/0},
        {docker, fun health_docker/0},
        {postgres, fun health_postgres/0},
        {kafka, fun health_kafka/0},
        {chrome, fun health_chrome/0}
    ],
    check_health_chain(HealthChecks).

-spec check_health_chain([{atom(), fun(() -> ok | {error, term()})}]) ->
          ok | {error, down, atom()}.
check_health_chain([]) ->
    ok;
check_health_chain([{Component, HealthFun} | Rest]) ->
    case HealthFun() of
        ok -> check_health_chain(Rest);
        _Error -> {error, down, Component}
    end.

%%% ====================== States ======================================

%% ---- starting_network ------------------------------------------------
starting_network(internal, start_network, S) ->
    lager:info("🌐 Checking network connectivity..."),
    slack_safe_info("🌐 Checking network connectivity..."),
    case health_network() of
        ok ->
            lager:info("✅ Network OK."),
            slack_safe_info("✅ Network OK."),
            S1 = reset_backoff(network, S),
            {next_state, starting_docker, S1#state{network_ok = true},
             [{next_event, internal, start_docker}]};
        {error, Reason} ->
            lager:error("❌ Network check failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Network check failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(network, S),
            {keep_state_and_data, [{state_timeout, Delay, retry_network}]}
    end;
starting_network(state_timeout, retry_network, S) ->
    {next_state, starting_network, S, [{next_event, internal, start_network}]}.

%% ---- starting_docker -------------------------------------------------
starting_docker(internal, start_docker, S) ->
    lager:info("🐳 Starting Docker compose..."),
    slack_safe_info("🐳 Starting Docker compose..."),
    case docker_srv:start_compose() of
        ok ->
            lager:info("✅ Docker started."),
            slack_safe_info("✅ Docker started."),
            S1 = reset_backoff(docker, S),
            {next_state, starting_postgres, S1#state{docker_ok = true},
             [{next_event, internal, start_postgres}]};
        {error, Reason} ->
            lager:error("❌ Docker failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Docker failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(docker, S),
            {keep_state_and_data, [{state_timeout, Delay, retry_docker}]}
    end;
starting_docker(state_timeout, retry_docker, S) ->
    {next_state, starting_docker, S, [{next_event, internal, start_docker}]}.

%% ---- starting_postgres -----------------------------------------------
starting_postgres(internal, start_postgres, S) ->
    lager:info("🧱 Starting PostgreSQL container..."),
    slack_safe_info("🧱 Starting PostgreSQL..."),
    case docker_srv:start_container("postgres-server") of
        ok ->
            lager:info("✅ PostgreSQL ready."),
            slack_safe_info("✅ PostgreSQL ready."),
            S1 = reset_backoff(postgres, S),
            {next_state, starting_kafka, S1#state{postgres_ok = true},
             [{next_event, internal, start_kafka}]};
        {error, Reason} ->
            lager:error("❌ PostgreSQL failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ PostgreSQL failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(postgres, S),
            {keep_state_and_data, [{state_timeout, Delay, retry_postgres}]}
    end;
starting_postgres(state_timeout, retry_postgres, S) ->
    {next_state, starting_postgres, S, [{next_event, internal, start_postgres}]}.

%% ---- starting_kafka --------------------------------------------------
starting_kafka(internal, start_kafka, S) ->
    lager:info("📡 Starting Kafka container..."),
    slack_safe_info("📡 Starting Kafka..."),
    case docker_srv:start_container("kafka-server") of
        ok ->
            lager:info("✅ Kafka started."),
            slack_safe_info("✅ Kafka started."),
            S1 = reset_backoff(kafka, S),
            {next_state, starting_chrome, S1#state{kafka_ok = true},
             [{next_event, internal, start_chrome}]};
        {error, Reason} ->
            lager:error("❌ Kafka failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Kafka failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(kafka, S),
            {keep_state_and_data, [{state_timeout, Delay, retry_kafka}]}
    end;
starting_kafka(state_timeout, retry_kafka, S) ->
    {next_state, starting_kafka, S, [{next_event, internal, start_kafka}]}.

%% ---- starting_chrome -------------------------------------------------
starting_chrome(internal, start_chrome, S) ->
    lager:info("🪞 Starting Chrome..."),
    slack_safe_info("🪞 Starting Chrome..."),
    case chrome_srv:is_chrome_running() of
        true ->
            lager:info("✅ Chrome is already running."),
            slack_safe_info("✅ Chrome is already running."),
            S2 = reset_backoff(chrome, S),
            lager:info("🚀 All resources ready."),
            slack_safe_info("🚀 All resources ready (Resource FSM READY). Notifying Service Orchestrator."),
            gen_statem:cast(service_orchestrator, {resources_ready, self()}),
            {next_state, ready, S2#state{chrome_ok = true, ready = true},
             [{state_timeout, ?MONITOR_MS, monitor}]};
        false ->
            case chrome_srv:start_chrome() of
                ok ->
                    lager:info("✅ Chrome started."),
                    slack_safe_info("✅ Chrome started."),
                    S2 = reset_backoff(chrome, S),
                    lager:info("🚀 All resources ready."),
                    slack_safe_info("🚀 All resources ready (Resource FSM READY). Notifying Service Orchestrator."),
                    gen_statem:cast(service_orchestrator, {resources_ready, self()}),
                    {next_state, ready, S2#state{chrome_ok = true, ready = true},
                     [{state_timeout, ?MONITOR_MS, monitor}]};
                {error, Reason} ->
                    lager:error("❌ Chrome failed: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ Chrome failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    {Delay, _S} = next_backoff(chrome, S),
                    {keep_state_and_data, [{state_timeout, Delay, retry_chrome}]}
            end
    end;
starting_chrome(state_timeout, retry_chrome, S) ->
    {next_state, starting_chrome, S, [{next_event, internal, start_chrome}]}.

%% ---- ready / monitoring ---------------------------------------------
ready(enter, _Old, S) ->
    %% when entering ready from recovery also schedule monitor
    {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};

ready({call, From}, get_status, S) ->
    gen_statem:reply(From, S),
    {keep_state, S};

ready(state_timeout, monitor, S) ->
    case check_health(S) of
        ok ->
            {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
        {error, down, kafka} ->
            lager:warning("⚠️ Kafka down, initiating recovery..."),
            slack_safe_error("⚠️ Kafka down, attempting recovery..."),
            {next_state, recovering_kafka, S, [{next_event, internal, recover_kafka}]};
        {error, down, postgres} ->
            lager:warning("⚠️ PostgreSQL down, initiating recovery..."),
            slack_safe_error("⚠️ PostgreSQL down, attempting recovery..."),
            {next_state, recovering_postgres, S, [{next_event, internal, recover_postgres}]};
        {error, down, docker} ->
            lager:warning("⚠️ Docker down, initiating recovery..."),
            slack_safe_error("⚠️ Docker down, attempting recovery..."),
            {next_state, recovering_docker, S, [{next_event, internal, recover_docker}]};
        {error, down, chrome} ->
            lager:warning("⚠️ Chrome down, initiating recovery..."),
            slack_safe_error("⚠️ Chrome down, attempting recovery..."),
            {next_state, recovering_chrome, S, [{next_event, internal, recover_chrome}]};
        {error, down, network} ->
            lager:warning("⚠️ Network down, re-checking pipeline..."),
            slack_safe_error("⚠️ Network down, re-checking pipeline..."),
            %% go back to starting_network pipeline
            {next_state, starting_network, S#state{ready=false}, [{next_event, internal, start_network}]}
    end;

ready(_Type, _Evt, S) ->
    {keep_state, S}.

%% ---- recovering_kafka ------------------------------------------------
recovering_kafka(internal, recover_kafka, S) ->
    case docker_srv:restart_container("kafka-server") of
        ok ->
            lager:info("✅ Kafka recovered."),
            slack_safe_info("✅ Kafka recovered."),
            S1 = reset_backoff(kafka, S),
            {next_state, ready, S1#state{kafka_ok=true}, [{state_timeout, ?MONITOR_MS, monitor}]};
        {error, Reason} ->
            lager:error("❌ Kafka recovery failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Kafka recovery failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, S1} = next_backoff(kafka, S),
            {keep_state, S1, [{state_timeout, Delay, recover_kafka}]}
    end.

%% ---- recovering_postgres --------------------------------------------
recovering_postgres(internal, recover_postgres, S) ->
    case docker_srv:restart_container("postgres-server") of
        ok ->
            lager:info("✅ PostgreSQL recovered."),
            slack_safe_info("✅ PostgreSQL recovered."),
            S1 = reset_backoff(postgres, S),
            {next_state, ready, S1#state{postgres_ok=true}, [{state_timeout, ?MONITOR_MS, monitor}]};
        {error, Reason} ->
            lager:error("❌ PostgreSQL recovery failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ PostgreSQL recovery failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(postgres, S),
            {keep_state_and_data, [{state_timeout, Delay, recover_postgres}]}
    end.

%% ---- recovering_docker ----------------------------------------------
recovering_docker(internal, recover_docker, S) ->
    case docker_srv:start_compose() of
        ok ->
            lager:info("✅ Docker up."),
            slack_safe_info("✅ Docker up."),
            S1 = reset_backoff(docker, S),
            {next_state, ready, S1#state{docker_ok=true}, [{state_timeout, ?MONITOR_MS, monitor}]};
        {error, Reason} ->
            lager:error("❌ Docker recovery failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Docker recovery failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, _S1} = next_backoff(docker, S),
            {keep_state_and_data, [{state_timeout, Delay, recover_docker}]}
    end.

%% ---- recovering_chrome ----------------------------------------------
recovering_chrome(internal, recover_chrome, S) ->
    case chrome_srv:start_chrome() of
        ok ->
            lager:info("✅ Chrome recovered."),
            slack_safe_info("✅ Chrome recovered."),
            S1 = reset_backoff(chrome, S),
            {next_state, ready, S1#state{chrome_ok=true}, [{state_timeout, ?MONITOR_MS, monitor}]};
        {error, Reason} ->
            lager:error("❌ Chrome recovery failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Chrome recovery failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            {Delay, S1} = next_backoff(chrome, S),
            {keep_state, S1, [{state_timeout, Delay, recover_chrome}]}
    end.