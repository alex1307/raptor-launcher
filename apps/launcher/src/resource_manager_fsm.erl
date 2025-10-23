%%% ====================================================================
%%%  Resource Orchestrator FSM
%%%  Orchestrates startup of infrastructure components via child FSMs:
%%%  Network → docker_fsm (Docker/Kafka/Postgres) → chrome_fsm (Chrome)
%%%
%%%  Features:
%%%   - Delegates Docker/Kafka/Postgres lifecycle to docker_fsm
%%%   - Delegates Chrome lifecycle to chrome_fsm
%%%   - Coordinates startup sequence
%%%   - Slack notifications on each step
%%%   - Notifies service_orchestrator when all resources ready
%%% ====================================================================

-module(resource_manager_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/0, status/0]).

%% gen_statem
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% state callbacks (state_functions mode)
-export([
    check_network/3,
    network_ok/3,
    ready/3
]).

-define(RETRY_MS,    5000).     %% base retry delay
-define(MAX_RETRIES, 10).

-record(state, {
    %% readiness flags
    network_ok  = false :: boolean(),
    docker_ok   = false :: boolean(),
    chrome_ok   = false :: boolean(),
    ready       = false :: boolean(),
    
    %% child FSM pids
    docker_fsm_pid = undefined :: pid() | undefined,
    chrome_fsm_pid = undefined :: pid() | undefined,
    
    retries = 0 :: non_neg_integer()
}).

%%% ====================== API =========================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_statem:call(?MODULE, get_status).

%%% ====================== gen_statem ==================================

callback_mode() -> [state_functions, state_enter].

init([]) ->
    lager:info("🔧 Resource Orchestrator initializing..."),
    slack_safe_info("🔧 Resource Orchestrator starting..."),
    {ok, check_network, #state{}}.

terminate(_Reason, _State, _Data) ->
    lager:info("Resource Orchestrator stopped."),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%% ====================== Helpers =====================================

%% Slack wrappers (don't crash if slack utils/env missing)
-spec slack_safe_info(string() | iodata()) -> ok.
-spec slack_safe_error(string() | iodata()) -> ok.

slack_safe_info(Msg) when is_list(Msg) ->
    lager:info("Slack: ~ts", [Msg]),
    try
        slack_utils:notify_info(Msg)
    catch
        _:Error ->
            lager:warning("Failed to send Slack notification: ~p", [Error]),
            ok
    end;
slack_safe_info(Msg) ->
    MsgStr = lists:flatten(io_lib:format("~ts", [Msg])),
    slack_safe_info(MsgStr).
    
slack_safe_error(Msg) when is_list(Msg) ->
    lager:error("Slack: ~ts", [Msg]),
    try
        slack_utils:notify_error(Msg)
    catch
        _:Error ->
            lager:warning("Failed to send Slack notification: ~p", [Error]),
            ok
    end;
slack_safe_error(Msg) ->
    MsgStr = lists:flatten(io_lib:format("~ts", [Msg])),
    slack_safe_error(MsgStr).

%% Retry helper
retry_or_fail(Retries, Component) ->
    if
        Retries >= ?MAX_RETRIES ->
            Msg = io_lib:format("❌ ~p failed after ~p retries", [Component, ?MAX_RETRIES]),
            slack_safe_error(Msg),
            {stop, {max_retries, Component}};
        true ->
            timer:sleep(?RETRY_MS),
            {keep_state_and_data, [{state_timeout, 0, check}]}
    end.

%% Component readiness helpers
ensure_docker_status(S) ->
    case whereis(docker_fsm) of
        undefined ->
            lager:warning("docker_fsm not started yet, retrying..."),
            {S#state{docker_ok = false, docker_fsm_pid = undefined}, 1000};
        Pid ->
            try gen_statem:call(Pid, get_status, 5000) of
                {State, _} when State =:= ready; State =:= monitoring ->
                    {maybe_mark_docker_ready(S, Pid), 0};
                _ ->
                    lager:info("docker_fsm not ready yet, retrying..."),
                    {mark_docker_not_ready(S, Pid), 2000}
            catch
                _:_ ->
                    lager:warning("docker_fsm call failed, retrying..."),
                    {mark_docker_not_ready(S, Pid), 2000}
            end
    end.

ensure_chrome_status(S) ->
    case whereis(chrome_fsm) of
        undefined ->
            lager:warning("chrome_fsm not started yet, retrying..."),
            {S#state{chrome_ok = false, chrome_fsm_pid = undefined}, 1000};
        Pid ->
            try gen_statem:call(Pid, get_status, 5000) of
                {State, _} when State =:= ready; State =:= monitoring ->
                    {maybe_mark_chrome_ready(S, Pid), 0};
                _ ->
                    lager:info("chrome_fsm not ready yet, retrying..."),
                    {mark_chrome_not_ready(S, Pid), 2000}
            catch
                _:_ ->
                    lager:warning("chrome_fsm call failed, retrying..."),
                    {mark_chrome_not_ready(S, Pid), 2000}
            end
    end.

maybe_mark_docker_ready(S, Pid) ->
    case S#state.docker_ok of
        true ->
            S#state{docker_fsm_pid = Pid};
        false ->
            lager:info("✅ docker_fsm is ready"),
            slack_safe_info("✅ Docker, Kafka, and Postgres are ready!"),
            S#state{docker_ok = true, docker_fsm_pid = Pid}
    end.

mark_docker_not_ready(S, Pid) ->
    S#state{docker_ok = false, docker_fsm_pid = Pid}.

maybe_mark_chrome_ready(S, Pid) ->
    case S#state.chrome_ok of
        true ->
            S#state{chrome_fsm_pid = Pid};
        false ->
            lager:info("✅ chrome_fsm is ready"),
            slack_safe_info("✅ Chrome is ready!"),
            S#state{chrome_ok = true, chrome_fsm_pid = Pid}
    end.

mark_chrome_not_ready(S, Pid) ->
    S#state{chrome_ok = false, chrome_fsm_pid = Pid}.

%%% ====================== States ======================================

%% ---- check_network ---------------------------------------------------
check_network(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_network(state_timeout, check, S) ->
    lager:info("🌐 Checking network connectivity..."),
    slack_safe_info("🌐 Checking network connectivity..."),
    case network_utils:check_all() of
        ok ->
            lager:info("✅ Network OK."),
            slack_safe_info("✅ Network OK."),
            {next_state, network_ok, S#state{network_ok = true, retries = 0}};
        {error, Reason} ->
            lager:error("❌ Network check failed: ~p", [Reason]),
            ErrorMsg = lists:flatten(io_lib:format("❌ Network check failed: ~p", [Reason])),
            slack_safe_error(ErrorMsg),
            NewRetries = S#state.retries + 1,
            retry_or_fail(NewRetries, network)
    end.

%% ---- network_ok ------------------------------------------------------
network_ok(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check_components}]};
network_ok(state_timeout, check_components, S0) ->
    lager:info("🔍 Checking component status..."),
    slack_safe_info("🔍 Checking infrastructure components..."),
    {S1, DockerDelay} = ensure_docker_status(S0#state{ready = false}),
    {S2, ChromeDelay} = ensure_chrome_status(S1),
    case {S2#state.docker_ok, S2#state.chrome_ok} of
        {true, true} ->
            lager:info("🚀 All resources ready."),
            slack_safe_info("🚀 All resources ready!"),
            {next_state, ready, S2#state{ready = true}};
        _ ->
            Delay = lists:max([DockerDelay, ChromeDelay, 1000]),
            {keep_state, S2, [{state_timeout, Delay, check_components}]}
    end;
network_ok({call, From}, get_status, S) ->
    gen_statem:reply(From, S),
    {keep_state, S};
network_ok(_Type, _Event, S) ->
    {keep_state, S}.

%% ---- ready -----------------------------------------------------------
ready(enter, _OldState, S) ->
    {keep_state, S};
ready({call, From}, get_status, S) ->
    gen_statem:reply(From, S),
    {keep_state, S};
ready(_Type, _Event, S) ->
    {keep_state, S}.
