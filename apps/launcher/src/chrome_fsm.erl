%%% ------------------------------------------------------------------
%%% Chrome FSM
%%% Manages Chrome browser lifecycle
%%% States represent actual system conditions:
%%%   check_chrome -> chrome_is_running -> ready -> monitoring
%%% ------------------------------------------------------------------

-module(chrome_fsm).
-behaviour(gen_statem).

-export([start_link/0, stop/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% state functions
-export([
    check_chrome/3,
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

%%% ====================== gen_statem ==================================

callback_mode() -> [state_functions, state_enter].

init([]) ->
    lager:info("chrome_fsm: initializing"),
    {ok, check_chrome, #state{}}.

terminate(_Reason, _StateName, _Data) ->
    lager:info("chrome_fsm: terminated"),
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

%% Check if Chrome is running
check_chrome(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, 0, check}]};
check_chrome(state_timeout, check, S) ->
    lager:info("chrome_fsm: checking if chrome is running"),
    slack_safe_info("🌐 Checking Chrome status..."),
    case chrome_srv:is_chrome_running() of
        true ->
            lager:info("chrome_fsm: chrome already running"),
            slack_safe_info("✅ Chrome is running"),
            {next_state, ready, S#state{retries = 0}};
        false ->
            lager:info("chrome_fsm: chrome not running, attempting to start"),
            slack_safe_info("🌐 Starting Chrome browser..."),
            case chrome_srv:start_chrome() of
                ok ->
                    slack_safe_info("✅ Chrome started successfully"),
                    {next_state, ready, S#state{retries = 0}};
                {error, Reason} ->
                    lager:error("chrome_fsm: failed to start chrome: ~p", [Reason]),
                    ErrorMsg = lists:flatten(io_lib:format("❌ Chrome failed: ~p", [Reason])),
                    slack_safe_error(ErrorMsg),
                    retry_or_fail(check_chrome, S)
            end
    end;
check_chrome(state_timeout, retry, S) ->
    {keep_state, S, [{state_timeout, 0, check}]}.

%% Chrome is ready, start monitoring
ready(enter, _OldState, S) ->
    lager:info("chrome_fsm: chrome ready, starting monitoring"),
    slack_safe_info("🎉 Chrome is ready and monitoring!"),
    {keep_state, S, [{state_timeout, 0, start_monitoring}]};
ready(state_timeout, start_monitoring, S) ->
    {next_state, monitoring, S, [{state_timeout, ?MONITOR_MS, monitor}]};
ready({call, From}, get_status, S) ->
    gen_statem:reply(From, {ready, S}),
    {keep_state, S};
ready(_Type, _Event, S) ->
    {keep_state, S}.

%% Continuously monitor Chrome health
monitoring(enter, _OldState, S) ->
    {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
monitoring(state_timeout, monitor, S) ->
    case chrome_srv:is_chrome_running() of
        true ->
            {keep_state, S, [{state_timeout, ?MONITOR_MS, monitor}]};
        false ->
            lager:warning("chrome_fsm: chrome down during monitoring, restarting check"),
            {next_state, check_chrome, S#state{retries = 0}}
    end;
monitoring({call, From}, get_status, S) ->
    gen_statem:reply(From, {monitoring, S}),
    {keep_state, S};
monitoring(_Type, _Event, S) ->
    {keep_state, S}.

%%% ====================== Helper Functions ============================

retry_or_fail(State, S = #state{retries = N}) when N < ?MAX_RETRIES ->
    lager:warning("chrome_fsm: retry ~p/~p for state ~p", [N+1, ?MAX_RETRIES, State]),
    {keep_state, S#state{retries = N+1}, [{state_timeout, ?RETRY_MS, retry}]};
retry_or_fail(State, S) ->
    lager:error("chrome_fsm: max retries reached in state ~p, staying in state", [State]),
    {keep_state, S, [{state_timeout, ?RETRY_MS * 2, retry}]}.
