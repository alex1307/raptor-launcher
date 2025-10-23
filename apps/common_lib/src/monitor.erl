-module(monitor).
-export([wait_for_service/1, wait_for_services/1]).

%% Функция която чака един автомат да приключи
wait_for_service(ServiceName) ->
    {ok, Pid} = raptor_service_fsm:start_link(ServiceName),
    MonitorRef = erlang:monitor(process, Pid),
    
    receive
        {'DOWN', MonitorRef, process, Pid, normal} ->
            {ok, success};
        {'DOWN', MonitorRef, process, Pid, {shutdown, Reason}} ->
            {error, Reason};
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            {error, Reason}
    end.

%% Функция която стартира scheduler и чака всички services да приключат
wait_for_services(ServicesList) ->
    {ok, SchedulerPid} = raptor_scheduler:start_link(),
    ok = raptor_scheduler:schedule_services(ServicesList),
    
    MonitorRef = erlang:monitor(process, SchedulerPid),
    
    %% Чакаме scheduler-а да приключи с всички services
    wait_for_scheduler_completion(SchedulerPid, MonitorRef).

%% Helper функция която чака scheduler-а
wait_for_scheduler_completion(SchedulerPid, MonitorRef) ->
    timer:sleep(5000), %% Проверяваме на всеки 5 секунди
    
    case erlang:is_process_alive(SchedulerPid) of
        true ->
            Status = raptor_scheduler:get_status(),
            case maps:get(current_service, Status) of
                undefined ->
                    %% Всички services са приключили
                    erlang:demonitor(MonitorRef, [flush]),
                    {ok, maps:get(results, Status)};
                _ ->
                    %% Има текущ service, чакаме още
                    wait_for_scheduler_completion(SchedulerPid, MonitorRef)
            end;
        false ->
            %% Scheduler-ът е терминиран
            {error, scheduler_terminated}
    end.