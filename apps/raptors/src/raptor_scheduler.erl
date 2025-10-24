%%% ====================================================================
%%%  Raptor Scheduler - стартира списък от services последователно
%%%  
%%%  Стартира всеки service чрез raptors_srv:start_service/1
%%%  и след това продължава със следващия.
%%%
%%%  Записва резултатите и праща обобщена Slack нотификация в края.
%%% ====================================================================

-module(raptor_scheduler).
-behaviour(gen_server).

%% API
-export([start_link/2, schedule_services/2, get_status/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    name :: atom(),                          %% Името на scheduler-а
    services_queue = [] :: [string()],       %% Опашка от services за изпълнение
    current_service = undefined :: undefined | string(),
    results = [] :: [{string(), ok | error, term()}],  %% История на резултатите
    started_at = undefined :: undefined | integer(),
    %% Periodic scheduling
    periodic_services = undefined :: undefined | [string()],
    periodic_interval_ms = undefined :: undefined | pos_integer(),
    periodic_timer_ref = undefined :: undefined | reference()
}).

%%% ====================== API =========================================

%% Стартира scheduler с име и периодичен интервал
%% Name - атом за регистрация (например crawler_scheduler)
%% IntervalMs - интервал в милисекунди (например 24 * 60 * 60 * 1000 за 24 часа)
start_link(Name, IntervalMs) when is_atom(Name), is_integer(IntervalMs), IntervalMs > 0 ->
    gen_server:start_link({local, Name}, ?MODULE, [IntervalMs], []).

%% Стартира списък от services последователно
%% Name - името на scheduler-а (атом)
%% ServicesList - списък от services за изпълнение
schedule_services(Name, ServicesList) when is_atom(Name), is_list(ServicesList) ->
    gen_server:call(Name, {schedule_services, ServicesList}).

%% Получава текущия статус на scheduler-а
get_status(Name) when is_atom(Name) ->
    gen_server:call(Name, get_status).

stop(Name) when is_atom(Name) ->
    gen_server:stop(Name).

%%% ====================== gen_server ==================================

init([IntervalMs]) ->
    %% Получаваме името на scheduler-а от регистрацията
    Name = case process_info(self(), registered_name) of
        {registered_name, RegisteredName} -> RegisteredName;
        _ -> undefined
    end,
    
    lager:debug("raptor_scheduler [~p]: initialized with interval ~p ms", [Name, IntervalMs]),
    
    %% Инициализираме DETS таблиците чрез state_utils
    case state_utils:init_tables() of
        ok ->
            %% Опитваме се да възстановим state от DETS
            RestoredState = restore_state_from_dets(Name, IntervalMs),
            {ok, RestoredState};
            
        {error, Reason} ->
            lager:error("raptor_scheduler [~p]: failed to initialize DETS tables: ~p", 
                       [Name, Reason]),
            {ok, #state{
                name = Name,
                periodic_interval_ms = IntervalMs
            }}
    end.

handle_call({schedule_services, ServicesList}, _From, State = #state{periodic_interval_ms = IntervalMs}) ->
    lager:debug("raptor_scheduler: scheduling ~p services (periodic with interval ~p ms)", 
               [length(ServicesList), IntervalMs]),
    
    %% Отменяме предишен timer ако има такъв
    NewState = cancel_timer(State),
    
    %% Запазваме periodic настройките
    NewState2 = NewState#state{
        periodic_services = ServicesList
    },
    
    %% Стартираме веднага първия цикъл
    NewState3 = start_periodic_cycle(NewState2),
    
    %% Запазваме state след настройка
    save_state(NewState3),
    
    {reply, ok, NewState3};

handle_call(get_status, _From, State = #state{
    services_queue = Queue,
    current_service = Current,
    results = Results,
    periodic_services = PeriodicServices,
    periodic_interval_ms = PeriodicInterval
}) ->
    Status = #{
        current_service => Current,
        remaining_services => Queue,
        completed_count => length(Results),
        results => Results,
        periodic_enabled => PeriodicServices =/= undefined,
        periodic_services => PeriodicServices,
        periodic_interval_ms => PeriodicInterval
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Timer за следващ periodic цикъл
handle_info(periodic_cycle, State) ->
    lager:debug("raptor_scheduler: starting periodic cycle"),
    NewState = start_periodic_cycle(State),
    save_state(NewState),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{name = Name, services_queue = Queue}) ->
    lager:debug("raptor_scheduler [~p]: terminating", [Name]),
    
    %% Ако има незавършени services, запазваме state (crash recovery)
    %% Ако няма, изтриваме state (нормално завършване)
    case Queue of
        [] ->
            %% Няма незавършени services - изтриваме crash recovery state
            clear_state(State);
        _ ->
            %% Има незавършени services - запазваме state за recovery
            save_state(State)
    end,
    
    %% Затваряме DETS таблиците чрез state_utils
    state_utils:close_tables(),
    
    ok.

%%% ====================== DETS Persistence Functions ==================

%% Възстановява state от DETS таблици чрез state_utils
restore_state_from_dets(Name, IntervalMs) ->
    %% Проверяваме дали има crash recovery state чрез state_utils
    case state_utils:restore_scheduler_state(Name) of
        {ok, SavedState} ->
            %% Имаме запазен state - имало е crash по време на изпълнение
            lager:debug("raptor_scheduler [~p]: found crash recovery state: ~p", [Name, SavedState]),
            
            #{
                services_queue := SavedQueue,
                current_service := CurrentService,
                results := SavedResults
            } = SavedState,
            
            %% Филтрираме services чрез state_utils
            FilteredQueue = state_utils:filter_services_by_timing(SavedQueue),
            
            lager:debug("raptor_scheduler [~p]: filtered queue from ~p to ~p services", 
                       [Name, length(SavedQueue), length(FilteredQueue)]),
            
            %% Връщаме възстановения state
            #state{
                name = Name,
                services_queue = FilteredQueue,
                current_service = CurrentService,
                results = SavedResults,
                periodic_interval_ms = IntervalMs
            };
            
        {error, not_found} ->
            %% Няма запазен state - нормално стартиране
            lager:debug("raptor_scheduler [~p]: no crash recovery state, starting fresh", [Name]),
            #state{
                name = Name,
                periodic_interval_ms = IntervalMs
            };
            
        {error, Reason} ->
            lager:error("raptor_scheduler [~p]: error restoring state: ~p", [Name, Reason]),
            #state{
                name = Name,
                periodic_interval_ms = IntervalMs
            }
    end.

%% Запазва state в DETS (само scheduler_state за crash recovery) чрез state_utils
save_state(#state{
    name = Name,
    services_queue = Queue, 
    current_service = CurrentService,
    results = Results
}) ->
    %% Запазваме scheduler state под ключ Name (за crash recovery)
    StateMap = #{
        services_queue => Queue,
        current_service => CurrentService,
        results => Results
    },
    state_utils:save_scheduler_state(Name, StateMap).

%% Изтрива запазения scheduler state след приключване на цикъла
clear_state(#state{name = Name}) ->
    lager:debug("raptor_scheduler [~p]: clearing crash recovery state", [Name]),
    state_utils:clear_scheduler_state(Name).

%%% ====================== Helper Functions ============================

%% Стартира САМО ПЪРВИЯ service от опашката (1 сървиз на цикъл)
%% След изпълнение ротира опашката - преместване на изпълнения в края
start_next_service(State = #state{services_queue = []}) ->
    %% Няма services в опашката
    lager:warning("raptor_scheduler: empty service queue, nothing to start"),
    State;

start_next_service(State = #state{services_queue = [ServiceName | Rest]}) ->
    lager:info("raptor_scheduler: starting service: ~s", [ServiceName]),

    %% Mark current service
    State1 = State#state{
        current_service = ServiceName
    },

    %% Start the service directly via raptors_srv
    StartResult = raptors_srv:start_service(ServiceName),
    
    %% Normalize result
    Normalized = case StartResult of
        ok -> {ServiceName, ok, success};
        {ok, _} -> {ServiceName, ok, success};
        {error, Reason} -> {ServiceName, error, Reason};
        Other -> {ServiceName, error, {unexpected_start_result, Other}}
    end,

    %% Update timing on success
    case Normalized of
        {_, ok, _} ->
            Now = erlang:system_time(second),
            IntervalSeconds = case State1#state.periodic_interval_ms of
                undefined -> 0;
                V -> V div 1000
            end,
            state_utils:update_service_timing(ServiceName, IntervalSeconds, Now);
        _ ->
            lager:warning("raptor_scheduler: service ~s failed to start, not updating timing", [ServiceName])
    end,

    %% ROTATE queue: преместване на изпълнения сървиз в края
    %% Това гарантира че на следващия цикъл ще стартира следващия сървиз
    RotatedQueue = Rest ++ [ServiceName],

    %% Save result
    NewResults = [Normalized | State1#state.results],
    NewState = State1#state{
        services_queue = RotatedQueue,  % ВАЖНО: ротирана опашка!
        results = NewResults, 
        current_service = undefined
    },
    
    %% Send summary (само за текущия изпълнен сървиз)
    send_final_summary(NewState),
    save_state(NewState),
    
    %% Schedule next cycle (след periodic_interval_ms ще стартира СЛЕДВАЩИЯ от опашката)
    schedule_next_periodic_cycle(NewState).

%% Праща обобщена Slack нотификация в края
send_final_summary(#state{results = Results, started_at = StartedAt}) ->
    Now = erlang:monotonic_time(millisecond),
    Duration = case StartedAt of
        undefined -> 0.0;
        _ -> (Now - StartedAt) / 1000
    end,
    
    Total = length(Results),
    Successful = length([R || {_, ok, _} = R <- Results]),
    Failed = Total - Successful,
    
    %% Създаваме списък с резултатите (само текст, без emoji)
    ResultsList = lists:reverse(Results),
    ResultsText = lists:map(fun({Name, Status, _Info}) ->
        case Status of
            ok -> lists:flatten(io_lib:format("OK: ~s", [Name]));
            error -> lists:flatten(io_lib:format("FAILED: ~s", [Name]))
        end
    end, ResultsList),
    
    %% Формираме съобщението (без emoji)
    Msg = lists:flatten(io_lib:format(
        "Scheduler Summary~n"
        "Total: ~p | Success: ~p | Failed: ~p~n"
        "Duration: ~.1f seconds~n~n~s",
        [Total, Successful, Failed, Duration, string:join(ResultsText, "\n")]
    )),
    
    send_slack_notification(Msg).

%% Helper за изпращане на Slack съобщение
send_slack_notification(Message) ->
    Result = slack_utils:notify_info(Message),
    case Result of
        {'EXIT', Reason} ->
            lager:warning("Failed to send Slack notification: ~p", [Reason]);
        _ ->
            lager:debug("Slack notification sent: ~s", [Message])
    end.

%%% ====================== Periodic Helpers ============================

%% Стартира periodic цикъл
start_periodic_cycle(State = #state{periodic_services = undefined}) ->
    %% Няма periodic настройки
    State;

start_periodic_cycle(State = #state{periodic_services = Services}) ->
    lager:debug("raptor_scheduler: starting periodic cycle with ~p services", [length(Services)]),
    
    %% Филтрираме services по timing чрез state_utils
    FilteredServices = state_utils:filter_services_by_timing(Services),
    
    lager:debug("raptor_scheduler: filtered to ~p services ready to run", [length(FilteredServices)]),
    
    %% Изчистваме резултати и стартираме цикъла
    Now = erlang:monotonic_time(millisecond),
    NewState = State#state{
        services_queue = FilteredServices,
        results = [],
        started_at = Now,
        periodic_timer_ref = undefined
    },
    
    %% Стартираме първия service (ако има)
    case FilteredServices of
        [] ->
            lager:debug("raptor_scheduler: no services ready to run, waiting for next cycle"),
            NewState;
        _ ->
            start_next_service(NewState)
    end.

%% Планира следващия periodic цикъл
schedule_next_periodic_cycle(State = #state{
    periodic_services = undefined
}) ->
    %% Няма periodic настройки, не правим нищо
    State;

schedule_next_periodic_cycle(State = #state{
    periodic_interval_ms = IntervalMs
}) ->
    lager:debug("raptor_scheduler: scheduling next periodic cycle in ~p ms", [IntervalMs]),
    
    %% Стартираме timer
    TimerRef = erlang:send_after(IntervalMs, self(), periodic_cycle),
    
    State#state{periodic_timer_ref = TimerRef}.

%% Отменя текущия timer
cancel_timer(State = #state{periodic_timer_ref = undefined}) ->
    State;

cancel_timer(State = #state{periodic_timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    State#state{periodic_timer_ref = undefined}.
