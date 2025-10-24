%%% ====================================================================
%%%  State Utils - DETS persistence utilities
%%%  
%%%  Общи функции за работа с DETS таблици за state persistence
%%%  и timing management.
%%%
%%%  Предоставя:
%%%  - Отваряне/затваряне на DETS таблици
%%%  - Запазване/четене/изтриване на state
%%%  - Service timing management (last_run, interval)
%%%  - Филтриране на services по timing
%%% ====================================================================

-module(state_utils).

%% Lifecycle operations
-export([
    init_tables/0,
    close_tables/0
]).

%% State persistence operations (scheduler state)
-export([
    save_scheduler_state/2,
    restore_scheduler_state/1,
    clear_scheduler_state/1,
    scheduler_state_exists/1
]).

%% Service timing operations (per-service timing)
-export([
    update_service_timing/3,
    get_service_timing/1,
    filter_services_by_timing/1,
    should_service_run/1,
    clear_service_timing/1
]).

-type state_key() :: term().
-type state_value() :: term().
-type service_name() :: string() | binary().

%% Internal table names - САМО тук се дефинират!
-define(SCHEDULER_STATE_TABLE, scheduler_state_table).
-define(SERVICE_TIMING_TABLE, service_timing_table).
-define(SCHEDULER_STATE_FILE, "/tmp/scheduler_state.dets").
-define(SERVICE_TIMING_FILE, "/tmp/service_timing.dets").

%%% ====================== Lifecycle Operations ========================

%% Инициализира двете DETS таблици
-spec init_tables() -> ok | {error, term()}.
init_tables() ->
    %% Отваряме scheduler_state таблица
    StateResult = dets:open_file(?SCHEDULER_STATE_TABLE, 
                                  [{file, ?SCHEDULER_STATE_FILE}, {type, set}]),
    
    %% Отваряме service_timing таблица
    TimingResult = dets:open_file(?SERVICE_TIMING_TABLE, 
                                   [{file, ?SERVICE_TIMING_FILE}, {type, set}]),
    
    case {StateResult, TimingResult} of
        {{ok, _}, {ok, _}} ->
            lager:debug("state_utils: initialized DETS tables successfully"),
            ok;
        {{error, Reason}, _} ->
            lager:error("state_utils: failed to open scheduler_state table: ~p", [Reason]),
            {error, {scheduler_state_table, Reason}};
        {_, {error, Reason}} ->
            lager:error("state_utils: failed to open service_timing table: ~p", [Reason]),
            {error, {service_timing_table, Reason}}
    end.

%% Затваря двете DETS таблици
-spec close_tables() -> ok.
close_tables() ->
    _ = (catch dets:close(?SCHEDULER_STATE_TABLE)),
    _ = (catch dets:close(?SERVICE_TIMING_TABLE)),
    lager:debug("state_utils: closed DETS tables"),
    ok.

%%% ====================== Scheduler State Operations ==================

%% Запазва scheduler state
-spec save_scheduler_state(Key :: state_key(), Value :: state_value()) -> 
    ok | {error, term()}.
save_scheduler_state(Key, Value) ->
    case dets:insert(?SCHEDULER_STATE_TABLE, {Key, Value}) of
        ok ->
            dets:sync(?SCHEDULER_STATE_TABLE),
            lager:debug("state_utils: saved scheduler state for key ~p", [Key]),
            ok;
        {error, Reason} = Error ->
            lager:error("state_utils: failed to save scheduler state for key ~p: ~p", 
                       [Key, Reason]),
            Error
    end.

%% Възстановява scheduler state
-spec restore_scheduler_state(Key :: state_key()) -> 
    {ok, state_value()} | {error, not_found} | {error, term()}.
restore_scheduler_state(Key) ->
    case dets:lookup(?SCHEDULER_STATE_TABLE, Key) of
        [{Key, Value}] ->
            lager:debug("state_utils: restored scheduler state for key ~p", [Key]),
            {ok, Value};
        [] ->
            lager:debug("state_utils: no scheduler state found for key ~p", [Key]),
            {error, not_found};
        {error, Reason} = Error ->
            lager:error("state_utils: failed to restore scheduler state for key ~p: ~p", 
                       [Key, Reason]),
            Error
    end.

%% Изтрива scheduler state
-spec clear_scheduler_state(Key :: state_key()) -> ok | {error, term()}.
clear_scheduler_state(Key) ->
    case dets:delete(?SCHEDULER_STATE_TABLE, Key) of
        ok ->
            dets:sync(?SCHEDULER_STATE_TABLE),
            lager:debug("state_utils: cleared scheduler state for key ~p", [Key]),
            ok;
        {error, Reason} = Error ->
            lager:error("state_utils: failed to clear scheduler state for key ~p: ~p", 
                       [Key, Reason]),
            Error
    end.

%% Проверява дали съществува scheduler state за даден ключ
-spec scheduler_state_exists(Key :: state_key()) -> boolean().
scheduler_state_exists(Key) ->
    case dets:lookup(?SCHEDULER_STATE_TABLE, Key) of
        [{Key, _Value}] -> true;
        [] -> false;
        {error, _} -> false
    end.

%%% ====================== Service Timing Operations ===================

%% Обновява timing информация за service (само ServiceName, IntervalSeconds, Timestamp)
-spec update_service_timing(
    ServiceName :: service_name(),
    IntervalSeconds :: pos_integer(),
    Timestamp :: integer()
) -> ok | {error, term()}.
update_service_timing(ServiceName, IntervalSeconds, Timestamp) 
  when is_integer(IntervalSeconds), IntervalSeconds > 0, is_integer(Timestamp) ->
    
    TimingInfo = #{
        last_run_timestamp => Timestamp,
        interval_seconds => IntervalSeconds
    },
    
    case dets:insert(?SERVICE_TIMING_TABLE, {ServiceName, TimingInfo}) of
        ok ->
            dets:sync(?SERVICE_TIMING_TABLE),
            lager:debug("state_utils: updated timing for ~p: last_run=~p, interval=~ps", 
                       [ServiceName, Timestamp, IntervalSeconds]),
            ok;
        {error, Reason} = Error ->
            lager:error("state_utils: failed to update timing for ~p: ~p", 
                       [ServiceName, Reason]),
            Error
    end.

%% Получава timing информация за service (само ServiceName)
-spec get_service_timing(ServiceName :: service_name()) -> 
    {ok, #{last_run_timestamp := integer(), interval_seconds := pos_integer()}} | 
    {error, not_found} | {error, term()}.
get_service_timing(ServiceName) ->
    case dets:lookup(?SERVICE_TIMING_TABLE, ServiceName) of
        [{ServiceName, TimingInfo}] ->
            {ok, TimingInfo};
        [] ->
            {error, not_found};
        {error, Reason} = Error ->
            lager:error("state_utils: failed to get timing for ~p: ~p", 
                       [ServiceName, Reason]),
            Error
    end.

%% Изтрива timing информация за service
-spec clear_service_timing(ServiceName :: service_name()) -> ok | {error, term()}.
clear_service_timing(ServiceName) ->
    case dets:delete(?SERVICE_TIMING_TABLE, ServiceName) of
        ok ->
            dets:sync(?SERVICE_TIMING_TABLE),
            lager:debug("state_utils: cleared timing for ~p", [ServiceName]),
            ok;
        {error, Reason} = Error ->
            lager:error("state_utils: failed to clear timing for ~p: ~p", 
                       [ServiceName, Reason]),
            Error
    end.

%% Филтрира services - връща само тези които трябва да се стартират
-spec filter_services_by_timing(Services :: [service_name()]) -> [service_name()].
filter_services_by_timing(Services) ->
    Now = erlang:system_time(second),
    
    lists:filter(fun(ServiceName) ->
        case should_service_run_internal(ServiceName, Now) of
            {true, Reason} ->
                lager:debug("state_utils: service ~p ready to run (~s)", [ServiceName, Reason]),
                true;
            {false, Reason} ->
                lager:debug("state_utils: service ~p skipped (~s)", [ServiceName, Reason]),
                false
        end
    end, Services).

%% Проверява дали service трябва да се стартира
-spec should_service_run(ServiceName :: service_name()) -> boolean().
should_service_run(ServiceName) ->
    Now = erlang:system_time(second),
    case should_service_run_internal(ServiceName, Now) of
        {true, _} -> true;
        {false, _} -> false
    end.

%%% ====================== Internal Helpers ============================

%% Вътрешна функция за проверка дали service трябва да се стартира
should_service_run_internal(ServiceName, Now) ->
    case dets:lookup(?SERVICE_TIMING_TABLE, ServiceName) of
        [{ServiceName, TimingInfo}] ->
            %% Има timing запис - проверяваме дали е време
            #{
                last_run_timestamp := LastRun,
                interval_seconds := Interval
            } = TimingInfo,
            
            NextRun = LastRun + Interval,
            
            if 
                Now >= NextRun ->
                    ElapsedSeconds = Now - LastRun,
                    Reason = io_lib:format("last run ~ps ago, interval ~ps", 
                                          [ElapsedSeconds, Interval]),
                    {true, lists:flatten(Reason)};
                true ->
                    RemainingSeconds = NextRun - Now,
                    Reason = io_lib:format("runs in ~ps", [RemainingSeconds]),
                    {false, lists:flatten(Reason)}
            end;
            
        [] ->
            %% Няма timing запис - първо стартиране
            {true, "first run"};
            
        {error, Reason} ->
            lager:error("state_utils: error checking timing for ~p: ~p", [ServiceName, Reason]),
            {true, "error reading timing, running anyway"}
    end.
