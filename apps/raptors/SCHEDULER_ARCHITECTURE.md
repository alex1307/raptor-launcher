# Raptor Scheduler - Модулна/Дистрибутивна Архитектура

## Архитектура с 2 DETS таблици

### 1. `service_timing` таблица (per-service, перманентна)

**Файл**: `/tmp/service_timing.dets`

**Структура**:

```erlang
%% Ключ: ServiceName (стринг)
{"crawler_service", #{
    last_run_timestamp => 1729678200,     %% erlang:system_time(second)
    interval_seconds => 86400             %% 24 часа
}}

{"mobile_de_service", #{
    last_run_timestamp => 1729678300,
    interval_seconds => 43200             %% 12 часа - различен интервал!
}}
```

**Предназначение**:

- Пази КОГА е изпълнен последно всеки service
- Определя КОГА може да се изпълни следващия път
- **Перманентна** - никога не се изтрива
- Един запис за **ВСЕКИ service** (не за scheduler)

### 2. `scheduler_state` таблица (per-scheduler, временна)

**Файл**: `/tmp/scheduler_state.dets`

**Структура**:

```erlang
%% Ключ: SchedulerName (атом)
{crawler_scheduler, #{
    services_queue => ["mobile_de_service", "other_service"],
    current_service => "crawler_service",
    results => [{"crawler_service", ok, success}]
}}
```

**Предназначение**:

- Crash recovery - продължи откъдето си спрял
- **Временна** - изтрива се след успешно приключване на цикъл
- Един запис за **ВСЕКИ scheduler**

## Как работи?

### Сценарий 1: Нормално periodic изпълнение

```erlang
%% Стартиране на scheduler
{ok, _} = raptor_scheduler:start_link(my_scheduler, 3600000), %% 1 час

%% Първи път - конфигуриране
raptor_scheduler:schedule_services(my_scheduler, ["s1", "s2", "s3"]).

%% Какво се случва:
1. Проверка service_timing за ВСЕКИ service:
   - "s1": няма запис → ✅ first run, добавя се
   - "s2": няма запис → ✅ first run, добавя се
   - "s3": няма запис → ✅ first run, добавя се

2. Filtered queue: ["s1", "s2", "s3"]

3. Стартира се "s1"
   - След успех → UPDATE service_timing("s1"):
     {last_run_timestamp => Now, interval_seconds => 3600}
   
4. Стартира се "s2"
   - След успех → UPDATE service_timing("s2")

5. Стартира се "s3"
   - След успех → UPDATE service_timing("s3")

6. Всички готови:
   - DELETE scheduler_state(my_scheduler)
   - START timer за 1 час

7. След 1 час - втори цикъл:
   - Проверка service_timing:
     * "s1": last_run преди 1h, interval 1h → ✅ време е
     * "s2": last_run преди 1h, interval 1h → ✅ време е
     * "s3": last_run преди 1h, interval 1h → ✅ време е
   - Filtered queue: ["s1", "s2", "s3"] - всички отново
```

### Сценарий 2: BEAM crash по време на изпълнение

```erlang
%% Цикъл започва в 10:00
1. "s1" ✅ завършва → service_timing("s1") updated
2. "s2" ✅ завършва → service_timing("s2") updated
3. **CRASH** (BEAM restart) в 10:15

%% BEAM рестартира в 10:20
4. Supervisor стартира scheduler отново
5. init/1 проверява:
   - scheduler_state(my_scheduler) СЪЩЕСТВУВА!
   - services_queue = ["s3"]  (само този остава)
   - results = [{"s1", ok}, {"s2", ok}]
   
6. Филтрира services_queue по timing:
   - "s3": няма timing (не е стартиран) → ✅ добавя се
   
7. Продължава:
   - Стартира "s3" (БЕЗ да повтаря s1 и s2!)
   - След успех: DELETE scheduler_state
   - Следващ цикъл след 1 час
```

### Сценарий 3: Различни интервали за различни services

```erlang
%% Конфигурация
{ok, _} = raptor_scheduler:start_link(multi_sched, 3600000), %% 1 час

raptor_scheduler:schedule_services(multi_sched, [
    "hourly_check",   %% Ще се изпълнява на всеки час
    "daily_report"    %% Ще се изпълнява веднъж дневно
]).

%% Първо изпълнение (10:00):
- "hourly_check" ✅ → timing: {last_run=10:00, interval=3600}
- "daily_report" ✅ → timing: {last_run=10:00, interval=86400}

%% Второ изпълнение (11:00):
- "hourly_check": last=10:00, now=11:00 → 1h минал → ✅ RUN
- "daily_report": last=10:00, now=11:00 → 1h от 24h → ❌ SKIP

Filtered queue: ["hourly_check"]

%% Трето изпълнение (12:00):
- "hourly_check": last=11:00, now=12:00 → 1h минал → ✅ RUN
- "daily_report": last=10:00, now=12:00 → 2h от 24h → ❌ SKIP

%% ...

%% 25-то изпълнение (следващия ден 11:00):
- "hourly_check": last=10:00 (вчера), now=11:00 (днес) → 25h → ✅ RUN
- "daily_report": last=10:00 (вчера), now=11:00 (днес) → 25h > 24h → ✅ RUN

Filtered queue: ["hourly_check", "daily_report"]
```

## Предимства

✅ **Модулно управление** - Всеки service си има собствена конфигурация  
✅ **Различни интервали** - s1 може 1h, s2 може 24h, s3 може 12h  
✅ **Защита от дубликати** - service_timing предпазва от многократно изпълнение  
✅ **Crash recovery** - scheduler_state запазва прогреса  
✅ **Споделени services** - Един service може да се използва от много schedulers  
✅ **Исторически данни** - service_timing остава завинаги  
✅ **BEAM restart safe** - При рестарт на VM, scheduler знае откъде да продължи

## API

### Стартиране на scheduler

```erlang
%% Name - atom() за регистрация
%% IntervalMs - periodic интервал в милисекунди
{ok, Pid} = raptor_scheduler:start_link(my_scheduler, 3600000).
```

### Конфигуриране на services

```erlang
%% ServicesList - списък от service names (strings)
raptor_scheduler:schedule_services(my_scheduler, [
    "crawler_service",
    "mobile_de_service",
    "health_check"
]).
```

### Проверка на статус

```erlang
Status = raptor_scheduler:get_status(my_scheduler).
%% #{
%%   current_service => "crawler_service",
%%   remaining_services => ["mobile_de_service"],
%%   completed_count => 1,
%%   results => [{"crawler_service", ok, success}],
%%   ...
%% }
```

## Дебъгване

### Проверка на service timing

```erlang
{ok, T} = dets:open_file(service_timing, [{file, "/tmp/service_timing.dets"}]).
dets:lookup(T, "crawler_service").
%% [{<<"crawler_service">>, 
%%   #{last_run_timestamp => 1729678200, interval_seconds => 86400}}]

%% Виж всички services
dets:match(T, '$1').

dets:close(T).
```

### Проверка на scheduler state

```erlang
{ok, S} = dets:open_file(scheduler_state, [{file, "/tmp/scheduler_state.dets"}]).
dets:lookup(S, my_scheduler).
%% [{my_scheduler, 
%%   #{services_queue => ["s2","s3"], 
%%     current_service => "s1",
%%     results => []}}]

dets:close(S).
```

## Важни бележки

1. **service_timing НИКОГА НЕ СЕ ИЗТРИВА** - дори scheduler-ът да не се използва месеци
2. **scheduler_state се изтрива** след всеки успешен цикъл
3. **IntervalMs на scheduler** определя колко често се ПРОВЕРЯВА, не колко често се ИЗПЪЛНЯВА service-а
4. **Всеки service** може да има различен interval (конфигурира се при update_service_timing)
5. **Споделяне на services** - Ако 2 schedulers управляват един service, timing-ът предпазва от дублиране
