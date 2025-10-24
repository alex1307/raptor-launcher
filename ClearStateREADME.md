# Clear State README

Ръководство за управление на scheduler state и service timing информация.

## Съдържание

- [Проверка на статус](#проверка-на-статус)
- [Изтриване на state](#изтриване-на-state)
- [DETS файлове](#dets-файлове)
- [Често срещани сценарии](#често-срещани-сценарии)

---

## Проверка на статус

### 1. Scheduler Runtime Status

```erlang
%% Статус на autouncle_crawler scheduler (6 crawlers: ro, fr, de, ch, it, pl, nl)
raptor_scheduler:get_status(autouncle_crawler_scheduler_sup).

%% Статус на mobile_bg_crawler scheduler (1 crawler: bg)
raptor_scheduler:get_status(mobile_bg_scheduler_sup).

%% Статус на raptor scheduler (1 service: raptor)
raptor_scheduler:get_status(raptor_scheduler_sup).
```

**Резултат:**

```erlang
#{
  current_service => "crawler-ro" | undefined,        % Текущ изпълняван сървиз
  remaining_services => ["crawler-fr", "crawler-de"], % Следващи в опашката (ротирана)
  completed_count => 1,                               % Брой завършени в текущия цикъл
  results => [{"crawler-ro", ok, success}],           % Резултати от изпълнение
  periodic_enabled => true,                           % Дали е периодичен
  periodic_services => ["crawler-ro", "crawler-fr"],  % Оригинален списък сървизи
  periodic_interval_ms => 86400000                    % Интервал: 24 часа
}
```

### 2. Service Timing (Persistent)

```erlang
%% Провери кога последно е стартиран конкретен сървиз
state_utils:get_service_timing("crawler-ro").
state_utils:get_service_timing("raptor").

%% Резултат:
%% {ok, #{last_run_timestamp => 1729777800, interval_seconds => 86400}}
%% или: {error, not_found} ако никога не е стартиран

%% Провери дали сървизът трябва да се стартира СЕГА
state_utils:should_service_run("crawler-ro").
%% Връща: true (готов за стартиране) или false (чака интервала)

%% Вземи ВСИЧКИ timing записи
dets:match_object(service_timing_table, '_').
%% Връща: [{"crawler-ro", #{...}}, {"crawler-fr", #{...}}, ...]
```

### 3. Scheduler State (Persistent Recovery)

```erlang
%% Запазен state за autouncle_crawler scheduler (за crash recovery)
state_utils:restore_scheduler_state(autouncle_crawler).

%% Запазен state за mobile_bg_crawler scheduler
state_utils:restore_scheduler_state(mobile_bg_crawler).

%% Запазен state за raptor scheduler
state_utils:restore_scheduler_state(raptor).

%% Резултат:
%% {ok, #{
%%   services_queue => ["crawler-fr", "crawler-de"],  % Ротирана опашка
%%   current_service => undefined,                    % Текущ изпълняван (ако има)
%%   results => [{"crawler-ro", ok, success}]         % Резултати от цикъла
%% }}
```

---

## Изтриване на state

### 1. Изтриване на Service Timing

**За конкретен сървиз:**

```erlang
%% Изтрий timing за raptor сървиз
state_utils:clear_service_timing("raptor").

%% Изтрий timing за crawler-ro
state_utils:clear_service_timing("crawler-ro").
```

**За всички crawler сървизи:**

```erlang
state_utils:clear_service_timing("crawler-ro").
state_utils:clear_service_timing("crawler-fr").
state_utils:clear_service_timing("crawler-de").
state_utils:clear_service_timing("crawler-ch").
state_utils:clear_service_timing("crawler-it").
state_utils:clear_service_timing("crawler-pl").
state_utils:clear_service_timing("crawler-nl").
state_utils:clear_service_timing("crawler-bg").
```

### 2. Изтриване на Scheduler State

```erlang
%% Изтрий scheduler state за raptor scheduler
state_utils:clear_scheduler_state(raptor).

%% Изтрий scheduler state за autouncle_crawler scheduler
state_utils:clear_scheduler_state(autouncle_crawler).

%% Изтрий scheduler state за mobile_bg_crawler scheduler
state_utils:clear_scheduler_state(mobile_bg_crawler).
```

### 3. Nuclear Option - Изтрий ВСИЧКО

**От Erlang Shell (докато приложението работи):**

```erlang
%% Изтрий ВСИЧКИ timing записи
dets:delete_all_objects(service_timing_table).
dets:sync(service_timing_table).

%% Изтрий ВСИЧКИ scheduler state записи
dets:delete_all_objects(scheduler_state_table).
dets:sync(scheduler_state_table).
```

**От Terminal (докато приложението НЕ работи):**

```bash
# Изтрий DETS файловете напълно
rm -f /tmp/service_timing.dets
rm -f /tmp/scheduler_state.dets

# При следващ старт ще се създадат празни таблици
```

---

## DETS файлове

### Локация

```bash
/tmp/service_timing.dets     # Timing информация за всеки сървиз
/tmp/scheduler_state.dets    # Scheduler state за crash recovery
```

### Преглед на файловете

```bash
# Провери дали съществуват
ls -lh /tmp/service_timing.dets
ls -lh /tmp/scheduler_state.dets

# Изтрий ги (само ако приложението НЕ работи!)
rm -f /tmp/service_timing.dets /tmp/scheduler_state.dets
```

### Структура

**service_timing_table:**

- Ключ: `ServiceName` (string)
- Стойност: `#{last_run_timestamp => UnixTimestamp, interval_seconds => Seconds}`

**scheduler_state_table:**

- Ключ: `SchedulerName` (atom: `raptor`, `autouncle_crawler`, `mobile_bg_crawler`)
- Стойност: `#{services_queue => [...], current_service => ..., results => [...]}`

---

## Често срещани сценарии

### Сценарий 1: Reset на конкретен сървиз

**Цел:** Искаш "raptor" сървиза да може да стартира веднага (без да чака 24 часа).

```erlang
%% 1. Изтрий timing-а (премахва ограничението за интервал)
state_utils:clear_service_timing("raptor").

%% 2. Проверка - сега трябва да върне true
state_utils:should_service_run("raptor").
%% Очакван резултат: true

%% 3. Ръчно стартиране (ако искаш да го тестваш веднага)
raptors_srv:start_service("raptor").
```

### Сценарий 2: Reset на scheduler опашката

**Цел:** Искаш autouncle_crawler scheduler-ът да започне отначало (от първия сървиз).

```erlang
%% 1. Изтрий scheduler state (нулира опашката и results)
state_utils:clear_scheduler_state(autouncle_crawler).

%% 2. Провери че е изтрит
state_utils:restore_scheduler_state(autouncle_crawler).
%% Очакван резултат: {error, not_found}

%% 3. Рестартирай scheduler-а (опционално)
%% При следващ periodic_cycle ще започне отначало с пълния списък сървизи
```

### Сценарий 3: Пълен reset на всички сървизи

**Цел:** Изтрий всичко и започни от чисто (clean slate).

```erlang
%% === От Erlang Shell ===

%% 1. Изтрий ВСИЧКИ timing записи
dets:delete_all_objects(service_timing_table).
dets:sync(service_timing_table).

%% 2. Изтрий ВСИЧКИ scheduler state записи
dets:delete_all_objects(scheduler_state_table).
dets:sync(scheduler_state_table).

%% 3. Проверка - таблиците трябва да са празни
dets:match_object(service_timing_table, '_').
dets:match_object(scheduler_state_table, '_').
%% Очакван резултат: [] (празни списъци)
```

**Или от Terminal (по-лесно):**

```bash
# 1. Спри приложението
# Ctrl+C два пъти или kill процеса

# 2. Изтрий DETS файловете
rm -f /tmp/service_timing.dets
rm -f /tmp/scheduler_state.dets

# 3. Стартирай отново
make run-dev
```

### Сценарий 4: Debugging - Провери какво се случва

```erlang
%% 1. Провери runtime статус на всички schedulers
raptor_scheduler:get_status(autouncle_crawler_scheduler_sup).
raptor_scheduler:get_status(mobile_bg_scheduler_sup).
raptor_scheduler:get_status(raptor_scheduler_sup).

%% 2. Провери timing за всички сървизи
lists:foreach(fun(Service) ->
    io:format("~s: ~p~n", [Service, state_utils:get_service_timing(Service)])
end, ["raptor", "crawler-ro", "crawler-fr", "crawler-de", 
      "crawler-ch", "crawler-it", "crawler-pl", "crawler-nl", "crawler-bg"]).

%% 3. Провери persistent state за всички schedulers
lists:foreach(fun(Scheduler) ->
    io:format("~p: ~p~n", [Scheduler, state_utils:restore_scheduler_state(Scheduler)])
end, [raptor, autouncle_crawler, mobile_bg_crawler]).

%% 4. Провери дали сървизите са готови за стартиране
lists:foreach(fun(Service) ->
    Ready = state_utils:should_service_run(Service),
    io:format("~s ready: ~p~n", [Service, Ready])
end, ["raptor", "crawler-ro", "crawler-fr"]).
```

### Сценарий 5: Ротация на опашката (ръчно тестване)

**Цел:** Разбери как работи ротацията на scheduler опашката.

```erlang
%% 1. Провери текущата опашка
#{remaining_services := Queue} = raptor_scheduler:get_status(autouncle_crawler_scheduler_sup).
io:format("Current queue: ~p~n", [Queue]).

%% 2. След изпълнение на 1 сървиз, опашката ще бъде ротирана:
%% Преди:  ["crawler-ro", "crawler-fr", "crawler-de"]
%% След:   ["crawler-fr", "crawler-de", "crawler-ro"]  <- crawler-ro отива в края

%% 3. След 24 часа (periodic_cycle), scheduler-ът ще стартира "crawler-fr"
%% След още 24 часа -> "crawler-de"
%% След още 24 часа -> "crawler-ro" (пак)
```

---

## Важни забележки

### ⚠️ Timing vs Scheduler State

- **Service Timing** (`service_timing_table`):
  - Запазва **кога** последно е стартиран сървиз
  - Използва се за проверка дали е минал интервалът (24 часа)
  - Persistent между рестарти

- **Scheduler State** (`scheduler_state_table`):
  - Запазва **какво** е състоянието на scheduler-а (опашка, results)
  - Използва се за crash recovery (ако приложението спре по време на изпълнение)
  - Persistent между рестарти

### ⚠️ Scheduler Ротация

- Всяка scheduler инстанция стартира **САМО 1 сървиз на цикъл**
- След изпълнение, опашката се **ротира** (изпълненият сървиз отива в края)
- На следващия цикъл (след 24 часа) се стартира **следващият** сървиз от опашката

**Пример с autouncle_crawler:**

```
Цикъл 1 (Ден 1): "crawler-ro"  -> Queue: ["crawler-fr", "crawler-de", "crawler-ch", ...]
Цикъл 2 (Ден 2): "crawler-fr"  -> Queue: ["crawler-de", "crawler-ch", "crawler-it", ..., "crawler-ro"]
Цикъл 3 (Ден 3): "crawler-de"  -> Queue: ["crawler-ch", "crawler-it", "crawler-pl", ..., "crawler-fr"]
...
Цикъл 7 (Ден 7): "crawler-nl"  -> Queue: ["crawler-ro", "crawler-fr", ..., "crawler-de"]
Цикъл 8 (Ден 8): "crawler-ro"  -> (пак от началото)
```

### ⚠️ Crash Recovery

При crash на приложението, scheduler-ите автоматично възстановяват state-а:

- Четат последното състояние от DETS
- Продължават от където са спрели (с ротираната опашка)
- Не губят информация за кои сървизи са стартирани

---

## Команди за бърза справка

```erlang
%% === ПРОВЕРКА ===
raptor_scheduler:get_status(raptor_scheduler_sup).
state_utils:get_service_timing("raptor").
state_utils:should_service_run("raptor").

%% === RESET НА КОНКРЕТЕН СЪРВИЗ ===
state_utils:clear_service_timing("raptor").
state_utils:clear_scheduler_state(raptor).

%% === RESET НА ВСИЧКО ===
dets:delete_all_objects(service_timing_table), dets:sync(service_timing_table).
dets:delete_all_objects(scheduler_state_table), dets:sync(scheduler_state_table).

%% === РЪЧНО СТАРТИРАНЕ ===
raptors_srv:start_service("raptor").
raptors_srv:is_service_running("raptor").
```

---

## Контакти и въпроси

За повече информация вижте:

- `apps/raptors/src/raptor_scheduler.erl` - scheduler логика
- `apps/common_lib/src/state_utils.erl` - DETS persistence utilities
- `apps/raptors/src/raptors_srv.erl` - service management API
