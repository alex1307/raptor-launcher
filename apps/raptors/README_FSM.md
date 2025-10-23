# Raptor Service FSM - Опростена версия

## Цел

Опростен FSM който **надеждно стартира един service**, **мониторира го** и **се терминира когато service-а приключи**.

## Архитектура

### States (Състояния)

```
    [starting] → [running] → [finished] → TERMINATE
         ↓           ↓
      (грешка)   (service спира)
         ↓           ↓
    [finished]   [finished]
```

1. **`starting`** - FSM-а се опитва да стартира service-а
   - При успех → преминава в `running`
   - При грешка → преминава в `finished`

2. **`running`** - Service-а работи, FSM-а го мониторира на всеки 10 секунди
   - Ако service-а работи → остава в `running`
   - Ако service-а приключи → преминава в `finished`

3. **`finished`** - Service-а е приключил, FSM-а се терминира след 1 секунда

### API

#### `start_link(ServiceName)`
Стартира FSM за даден service.

```erlang
{ok, Pid} = raptor_service_fsm:start_link("my_service").
```

#### `get_status(Pid)`
Връща текущия статус на FSM-а.

```erlang
Status = raptor_service_fsm:get_status(Pid).
```

Примерен резултат:
```erlang
#{
    service_name => "my_service",
    state => running,              % starting | running | finished
    started_at => 123456789,       % monotonic time when started
    uptime_ms => 5000,             % milliseconds since start
    uptime_seconds => 5.0          % seconds since start
}
```

### Как да проверим дали FSM-а е активен?

```erlang
%% Вариант 1: Проверка дали процесът е жив
erlang:is_process_alive(Pid).

%% Вариант 2: Проверка на state
Status = raptor_service_fsm:get_status(Pid),
State = maps:get(state, Status),
case State of
    running -> io:format("Service is running~n");
    finished -> io:format("Service finished~n");
    starting -> io:format("Service is starting~n")
end.
```

## Интеграция със Scheduler

Scheduler-ът трябва да:

1. **Стартира FSM** за всеки service от списъка
2. **Запомня Pid-а** на всеки FSM
3. **Проверява статуса** чрез `get_status/1` или `is_process_alive/1`
4. **Чака FSM-а да се терминира** преди да стартира следващия service

### Пример за Scheduler логика

```erlang
%% Псевдокод за scheduler
start_services_sequentially([ServiceName | Rest]) ->
    %% Стартираме FSM за service-а
    {ok, Pid} = raptor_service_fsm:start_link(ServiceName),
    
    %% Чакаме FSM-а да се терминира
    MonitorRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            lager:info("Service ~s finished, starting next", [ServiceName]),
            start_services_sequentially(Rest)
    end;

start_services_sequentially([]) ->
    lager:info("All services completed").
```

## Промени спрямо стария код

### ❌ Премахнато:
- 24-часов интервал за рестартиране
- Slack алерти при uptime > 1 час
- `stopped` състояние
- `checking` състояние
- Сложна логика за timestamp-ове

### ✅ Добавено:
- Просто състояние `starting` → `running` → `finished`
- Бърза проверка на всеки 10 секунди
- Автоматично терминиране при край
- По-ясен статус API

## Тестване

```erlang
%% В Erlang shell:
rebar3 shell

%% Тестваме с примерен service
raptor_service_fsm_example:test_service("test_service").
```

## Бъдещи подобрения (за Scheduler)

1. **Scheduler модул** - управлява списък от services
2. **Периодично стартиране** - schedule на base на cron или интервал
3. **Логване на резултати** - записва кога всеки service е стартиран/приключен
4. **Error handling** - retry логика ако service не стартира
