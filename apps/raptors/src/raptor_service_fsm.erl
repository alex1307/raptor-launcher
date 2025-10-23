%%% ====================================================================
%%%  Raptor Service FSM - управлява един service (опростен вариант)
%%%  
%%%  При старт:
%%%  - Стартира service-а
%%%  - Мониторира дали работи
%%%  - Терминира се когато service-а приключи
%%%
%%%  Scheduler-ът трябва да може да разбере дали FSM-а е:
%%%  - running - сървиза работи
%%%  - finished - сървиза приключи, FSM-а ще се терминира
%%% ====================================================================

-module(raptor_service_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/1, get_status/1, wait_for_completion/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% State functions
-export([
    starting/3,
    running/3,
    finished/3
]).

-define(CHECK_INTERVAL_MS, 10 * 1000).  %% Проверяваме на всеки 10 секунди
-define(MAX_RETRIES, 3).                %% Максимален брой retry-та
-define(RETRY_DELAY_MS, 30 * 1000).    %% Изчакване между retry-та (30 секунди)

-record(state, {
    service_name :: string(),
    started_at = undefined :: undefined | integer(),
    retry_count = 0 :: non_neg_integer(),
    last_error = undefined :: undefined | term()
}).

%%% ====================== API =========================================

start_link(ServiceName) ->
    gen_statem:start_link(?MODULE, [ServiceName], []).

get_status(Pid) ->
    gen_statem:call(Pid, get_status).

%% Стартира service и чака да приключи
%% Връща: {ok, success} | {error, Reason}
wait_for_completion(ServiceName) ->
    {ok, Pid} = start_link(ServiceName),
    MonitorRef = erlang:monitor(process, Pid),
    
    receive
        {'DOWN', MonitorRef, process, Pid, normal} ->
            {ok, success};
        {'DOWN', MonitorRef, process, Pid, {shutdown, Reason}} ->
            {error, Reason};
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            {error, Reason}
    end.

%%% ====================== gen_statem ==================================

callback_mode() -> [state_functions, state_enter].

init([ServiceName]) ->
    lager:info("[~s] raptor_service_fsm: initializing", [ServiceName]),
    State = #state{service_name = ServiceName},
    {ok, starting, State}.

terminate(_Reason, _StateName, #state{service_name = Name}) ->
    lager:info("[~s] raptor_service_fsm: terminated", [Name]),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%% ====================== States ======================================

%% starting - стартираме service-а
starting(enter, _OldState, S = #state{service_name = Name, retry_count = Retry}) ->
    case Retry of
        0 -> 
            lager:info("[~s] starting service (first attempt)", [Name]);
        _ -> 
            lager:info("[~s] retrying service start (attempt ~p/~p)", [Name, Retry + 1, ?MAX_RETRIES + 1])
    end,
    {keep_state, S, [{state_timeout, 0, start}]};

starting(state_timeout, start, S = #state{service_name = Name, retry_count = Retry}) ->
    case start_service(Name) of
        ok ->
            lager:info("[~s] service started successfully", [Name]),
            Now = erlang:monotonic_time(millisecond),
            NewState = S#state{
                started_at = Now
            },
            {next_state, running, NewState};
        {error, Reason} ->
            lager:error("[~s] failed to start service: ~p", [Name, Reason]),
            
            %% Slack нотификация за грешка
            send_slack_error(Name, Reason, Retry + 1),
            
            %% Проверяваме дали можем да retry-нем
            if
                Retry < ?MAX_RETRIES ->
                    lager:warning("[~s] will retry after ~p ms", [Name, ?RETRY_DELAY_MS]),
                    NewState = S#state{
                        retry_count = Retry + 1,
                        last_error = Reason
                    },
                    {keep_state, NewState, [{state_timeout, ?RETRY_DELAY_MS, start}]};
                true ->
                    lager:error("[~s] max retries reached, giving up", [Name]),
                    send_slack_final_failure(Name, Reason, ?MAX_RETRIES + 1),
                    NewState = S#state{last_error = Reason},
                    {next_state, finished, NewState, [{state_timeout, 0, terminate_with_error}]}
            end
    end;

starting({call, From}, get_status, S) ->
    Status = build_status_map(starting, S),
    gen_statem:reply(From, Status),
    {keep_state, S};

starting(_Type, _Event, S) ->
    {keep_state, S}.

%% running - service-а работи, мониторираме го
running(enter, _OldState, S = #state{service_name = Name}) ->
    lager:info("[~s] service is running, monitoring", [Name]),
    {keep_state, S, [{state_timeout, ?CHECK_INTERVAL_MS, monitor}]};

running(state_timeout, monitor, S = #state{service_name = Name}) ->
    IsRunning = is_service_running(Name),
    
    case IsRunning of
        true ->
            %% Service все още работи
            {keep_state, S, [{state_timeout, ?CHECK_INTERVAL_MS, monitor}]};
        false ->
            %% Service е приключил успешно
            lager:info("[~s] service finished successfully", [Name]),
            send_slack_success(Name),
            {next_state, finished, S}
    end;

running({call, From}, get_status, S) ->
    Status = build_status_map(running, S),
    gen_statem:reply(From, Status),
    {keep_state, S};

running(_Type, _Event, S) ->
    {keep_state, S}.

%% finished - service-а е приключил, FSM-а ще се терминира
finished(enter, _OldState, S = #state{service_name = Name, last_error = Error}) ->
    case Error of
        undefined ->
            lager:info("[~s] service finished successfully, FSM will terminate", [Name]);
        _ ->
            lager:error("[~s] service finished with error: ~p, FSM will terminate", [Name, Error])
    end,
    %% Даваме време за get_status заявки преди да се терминираме
    {keep_state, S, [{state_timeout, 1000, terminate}]};

finished(state_timeout, terminate, S = #state{last_error = Error}) ->
    case Error of
        undefined -> {stop, normal, S};
        _ -> {stop, {shutdown, Error}, S}
    end;

finished(state_timeout, terminate_with_error, S = #state{last_error = Error}) ->
    {stop, {shutdown, Error}, S};

finished({call, From}, get_status, S) ->
    Status = build_status_map(finished, S),
    gen_statem:reply(From, Status),
    {keep_state, S};

finished(_Type, _Event, S) ->
    {keep_state, S}.

%%% ====================== Helper Functions ============================

%% Проверява дали service-а работи
is_service_running(ServiceName) ->
    try
        raptors_srv:is_service_running(ServiceName)
    catch
        _:Error ->
            lager:error("[~s] error checking if running: ~p", [ServiceName, Error]),
            false
    end.

%% Стартира service
start_service(ServiceName) ->
    try
        case raptors_srv:start_service(ServiceName) of
            {ok, _} -> ok;
            ok -> ok;
            {error, Reason} -> {error, Reason};
            Other -> {error, {unexpected, Other}}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% Създава status map за get_status
%% CurrentState - атом (starting, running, finished)
build_status_map(CurrentState, #state{
    service_name = Name, 
    started_at = StartedAt,
    retry_count = Retry,
    last_error = Error
}) ->
    Now = erlang:monotonic_time(millisecond),
    
    Uptime = case StartedAt of
        undefined -> undefined;
        _ -> Now - StartedAt
    end,
    
    #{
        service_name => Name,
        state => CurrentState,
        started_at => StartedAt,
        uptime_ms => Uptime,
        uptime_seconds => case Uptime of
            undefined -> undefined;
            _ -> Uptime / 1000
        end,
        retry_count => Retry,
        last_error => Error
    }.

%%% ====================== Slack Notifications =========================

%% Праща Slack съобщение при успешно приключване
send_slack_success(ServiceName) ->
    Msg = io_lib:format("✅ Service *~s* completed successfully", [ServiceName]),
    send_slack_notification(lists:flatten(Msg)).

%% Праща Slack съобщение при грешка (на всеки retry)
send_slack_error(ServiceName, Reason, AttemptNum) ->
    Msg = io_lib:format("⚠️ Service *~s* failed (attempt ~p): ~p", 
                        [ServiceName, AttemptNum, Reason]),
    send_slack_notification(lists:flatten(Msg)).

%% Праща Slack съобщение при окончателна грешка (след всички retry-та)
send_slack_final_failure(ServiceName, Reason, TotalAttempts) ->
    Msg = io_lib:format("❌ Service *~s* failed permanently after ~p attempts: ~p", 
                        [ServiceName, TotalAttempts, Reason]),
    send_slack_notification(lists:flatten(Msg)).

%% Helper за изпращане на Slack съобщение
send_slack_notification(Message) ->
    try
        slack_utils:notify_info(Message),
        lager:info("Slack notification sent: ~s", [Message])
    catch
        _:Error ->
            lager:warning("Failed to send Slack notification: ~p", [Error])
    end.
