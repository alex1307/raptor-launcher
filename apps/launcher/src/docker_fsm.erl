%%% ------------------------------------------------------------------
%%% Docker FSM
%%% Manages Docker, Kafka and Postgres lifecycle
%%% States represent actual system conditions:
%%%   check_docker -> docker_is_running -> check_kafka -> kafka_is_running ->
%%%   kafka_is_configured -> check_postgres -> postgres_is_running -> ready -> monitoring
%%% ------------------------------------------------------------------

-module(docker_fsm).
-behaviour(gen_statem).

-export([start_link/0, stop/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% state functions
-export([
    check_docker/3,
    check_kafka/3,
    kafka_is_configured/3,
    check_postgres/3,
    ready/3,
    monitoring/3
]).

-define(MONITOR_MS, 60_000).
-define(RETRY_MS, 30_000).
-define(MAX_RETRIES, 10).

-type component_status() :: up | down | unknown.

-record(state, {
    retries = 0 :: non_neg_integer(),
    docker_status = unknown :: component_status(),
    kafka_status = unknown :: component_status(),
    postgres_status = unknown :: component_status(),
    kafka_configured = false :: boolean()
}).

%%% ====================== API =========================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

%%% ====================== gen_statem ==================================

callback_mode() -> [state_functions, state_enter].

init([]) ->
    lager:info("docker_fsm: initializing"),
    {ok, check_docker, #state{}}.

terminate(_Reason, _StateName, _Data) ->
    lager:info("docker_fsm: terminated"),
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

status_from_bool(true) -> up;
status_from_bool(false) -> down.

maybe_notify_change(Status, Status, _UpMsg, _DownMsg) ->
    ok;
maybe_notify_change(unknown, down, _UpMsg, _DownMsg) ->
    ok;
maybe_notify_change(_Old, up, UpMsg, _DownMsg) ->
    slack_safe_info(UpMsg);
maybe_notify_change(_Old, down, _UpMsg, DownMsg) ->
    slack_safe_error(DownMsg).

set_docker_status(S = #state{docker_status = Old}, Status) ->
    maybe_notify_change(Old, Status, "✅ Docker is running", "❌ Docker is down"),
    S#state{docker_status = Status}.

set_kafka_status(S = #state{kafka_status = Old}, Status) ->
    maybe_notify_change(Old, Status, "✅ Kafka is running", "❌ Kafka is down"),
    S#state{kafka_status = Status}.

set_postgres_status(S = #state{postgres_status = Old}, Status) ->
    maybe_notify_change(Old, Status, "✅ Postgres is running", "❌ Postgres is down"),
    S#state{postgres_status = Status}.

set_kafka_configured(S = #state{kafka_configured = Old}, Configured) when Old =:= Configured ->
    S;
set_kafka_configured(S, true) ->
    slack_safe_info("✅ Kafka topics configured"),
    S#state{kafka_configured = true};
set_kafka_configured(S, false) ->
    S#state{kafka_configured = false}.

reset_retries(S) ->
    S#state{retries = 0}.

%%% ====================== State Functions =============================

%% Check if Docker is running
check_docker(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_docker(state_timeout, check, S0) ->
    lager:info("docker_fsm: checking if docker is running"),
    case docker_srv:is_docker_running() of
        true ->
            lager:info("docker_fsm: docker already running"),
            S1 = set_docker_status(S0, up),
            {next_state, check_kafka, reset_retries(S1)};
        false ->
            lager:info("docker_fsm: docker not running, attempting to start"),
            S1 = set_docker_status(S0, down),
            case docker_srv:start_compose() of
                ok ->
                    S2 = set_docker_status(S1, up),
                    {next_state, check_kafka, reset_retries(S2)};
                {error, Reason} ->
                    lager:error("docker_fsm: failed to start docker: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ Docker failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    retry_or_fail(check_docker, S1)
            end
    end;
check_docker(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check}]}.

%% Check if Kafka container is running
check_kafka(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_kafka(state_timeout, check, S0) ->
    lager:info("docker_fsm: checking if kafka is running"),
    case docker_srv:is_container_running("kafka-server") of
        true ->
            lager:info("docker_fsm: kafka already running"),
            S1 = set_kafka_status(S0, up),
            {next_state, kafka_is_configured, reset_retries(S1)};
        false ->
            lager:info("docker_fsm: kafka not running, attempting to start"),
            S1 = set_kafka_status(S0, down),
            case docker_srv:start_container("kafka-server") of
                ok ->
                    timer:sleep(2000), %% brief wait for startup
                    S2 = set_kafka_status(S1, up),
                    {next_state, kafka_is_configured, reset_retries(S2)};
                {error, Reason} ->
                    lager:error("docker_fsm: failed to start kafka: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ Kafka failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    retry_or_fail(check_kafka, S1)
            end
    end;
check_kafka(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check}]}.

%% Ensure Kafka is properly configured (topics, etc.)
kafka_is_configured(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
kafka_is_configured(state_timeout, check, S = #state{kafka_configured = true}) ->
    lager:info("docker_fsm: kafka configuration already applied, skipping"),
    %% Notify mobile_de_fsm even if config was already applied
    notify_mobile_de_kafka_ready(),
    {next_state, check_postgres, reset_retries(S)};
kafka_is_configured(state_timeout, check, S0) ->
    lager:info("docker_fsm: verifying kafka configuration"),
    case ensure_kafka_configuration() of
        ok ->
            lager:info("docker_fsm: kafka configuration verified"),
            %% Notify mobile_de_fsm that Kafka is ready
            notify_mobile_de_kafka_ready(),
            S1 = set_kafka_configured(S0, true),
            {next_state, check_postgres, reset_retries(S1)};
        {error, Reason} ->
            lager:error("docker_fsm: kafka configuration failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Kafka configuration failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            retry_or_fail(kafka_is_configured, S0)
    end;
kafka_is_configured(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check}]}.

%% Check if Postgres container is running
check_postgres(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_postgres(state_timeout, check, S0) ->
    lager:info("docker_fsm: checking if postgres is running"),
    case docker_srv:is_container_running("postgres-server") of
        true ->
            lager:info("docker_fsm: postgres already running"),
            S1 = set_postgres_status(S0, up),
            {next_state, ready, reset_retries(S1)};
        false ->
            lager:info("docker_fsm: postgres not running, attempting to start"),
            S1 = set_postgres_status(S0, down),
            case docker_srv:start_container("postgres-server") of
                ok ->
                    timer:sleep(2000), %% brief wait for startup
                    S2 = set_postgres_status(S1, up),
                    {next_state, ready, reset_retries(S2)};
                {error, Reason} ->
                    lager:error("docker_fsm: failed to start postgres: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ Postgres failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    retry_or_fail(check_postgres, S1)
            end
    end;
check_postgres(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check}]}.

%% All components are ready, start monitoring
ready(enter, _OldState, S) ->
    lager:info("docker_fsm: all components ready, starting monitoring"),
    slack_safe_info("🎉 Docker, Kafka, and Postgres are all ready!"),
    {keep_state, S, [{state_timeout, 0, start_monitoring}]};
ready(state_timeout, start_monitoring, S) ->
    {next_state, monitoring, S#state{retries = 0}, [{state_timeout, 0, monitor}]};
ready({call, From}, get_status, S) ->
    gen_statem:reply(From, {ready, S}),
    {keep_state, S};
ready(_Type, _Event, S) ->
    {keep_state, S}.

%% Continuously monitor health of all components
monitoring(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
monitoring(state_timeout, monitor, S0) ->
    DockerRunning = docker_srv:is_docker_running(),
    KafkaRunning = docker_srv:is_container_running("kafka-server"),
    PostgresRunning = docker_srv:is_container_running("postgres-server"),
    S1 = set_docker_status(S0, status_from_bool(DockerRunning)),
    S2 = set_kafka_status(S1, status_from_bool(KafkaRunning)),
    S3 = set_postgres_status(S2, status_from_bool(PostgresRunning)),
    case {DockerRunning, KafkaRunning, PostgresRunning} of
        {true, true, true} ->
            {keep_state, S3, [{state_timeout, ?MONITOR_MS, monitor}]};
        {false, _, _} ->
            lager:warning("docker_fsm: docker down during monitoring, restarting flow"),
            {next_state, check_docker, reset_retries(S3)};
        {true, false, _} ->
            lager:warning("docker_fsm: kafka down during monitoring, restarting from kafka check"),
            {next_state, check_kafka, reset_retries(S3)};
        {true, true, false} ->
            lager:warning("docker_fsm: postgres down during monitoring, restarting from postgres check"),
            {next_state, check_postgres, reset_retries(S3)}
    end;
monitoring({call, From}, get_status, S) ->
    gen_statem:reply(From, {monitoring, S}),
    {keep_state, S};
monitoring(_Type, _Event, S) ->
    {keep_state, S}.

%%% ====================== Helper Functions ============================

retry_or_fail(State, S = #state{retries = N}) when N < ?MAX_RETRIES ->
    lager:warning("docker_fsm: retry ~p/~p for state ~p", [N+1, ?MAX_RETRIES, State]),
    {keep_state, S#state{retries = N+1}, [{state_timeout, ?RETRY_MS, retry}]};
retry_or_fail(State, S) ->
    lager:error("docker_fsm: max retries reached in state ~p, staying in state", [State]),
    {keep_state, S, [{state_timeout, ?RETRY_MS * 2, retry}]}.

ensure_kafka_configuration() ->
    case kafka_srv:configure_all_topics() of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason};
        {ok, _Msg} ->
            %% Kafka might return {ok, "some output"} - treat as success if no error
            ok;
        Other ->
            lager:warning("docker_fsm: unexpected kafka config result: ~p", [Other]),
            {error, Other}
    end.

%% Notify mobile_de_fsm that Kafka is ready
notify_mobile_de_kafka_ready() ->
    case whereis(mobile_de_fsm) of
        undefined ->
            lager:warning("docker_fsm: mobile_de_fsm not started yet, cannot notify");
        Pid when is_pid(Pid) ->
            lager:info("docker_fsm: notifying mobile_de_fsm that Kafka is ready"),
            mobile_de_fsm:kafka_ready()
    end.
