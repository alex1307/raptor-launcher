# Raptor Scheduler

Scheduler който стартира списък от services **последователно** (един по един) и чака всеки да приключи.

## Как работи

### Режим 1: Веднъж (One-time)
1. **Получава списък** от service names
2. **Стартира първия** service чрез `raptor_service_fsm`
3. **Чака** FSM-а да се терминира (успех или грешка)
4. **Записва резултата** (success/error)
5. **Стартира следващия** service
6. **Повтаря** стъпки 3-5 докато не свършат всички services
7. **Праща Slack обобщение** с резултатите

### Режим 2: Периодично (Periodic)
1. **Получава списък** от service names и **интервал**
2. **Стартира цикъл** от всички services (като при one-time)
3. **Чака всички** services да приключат
4. **Изчаква интервала** (например 24 часа)
5. **Повтаря** от стъпка 2 безкрайно

## API

### `start_link/0`
Стартира scheduler-а като gen_server:
```erlang
{ok, Pid} = raptor_scheduler:start_link().
```

### `schedule_services/1`
Стартира списък от services последователно (веднъж):
```erlang
Services = ["crawler", "mobile_de", "raptor"],
ok = raptor_scheduler:schedule_services(Services).
```

### `schedule_periodic/2`
Стартира списък от services периодично на определен интервал:
```erlang
Services = ["crawler", "mobile_de", "raptor"],
IntervalMs = 24 * 60 * 60 * 1000,  %% 24 часа
ok = raptor_scheduler:schedule_periodic(Services, IntervalMs).
```

### `cancel_periodic/0`
Спира периодичното стартиране:
```erlang
ok = raptor_scheduler:cancel_periodic().
```

### `get_status/0`
Получава текущия статус:
```erlang
Status = raptor_scheduler:get_status().
%% => #{
%%     current_service => "crawler",
%%     remaining_services => ["mobile_de", "raptor"],
%%     completed_count => 0,
%%     results => []
%% }
```

### `stop/0`
Спира scheduler-а:
```erlang
ok = raptor_scheduler:stop().
```

## Примери за използване

### Пример 1: Веднъж (One-time)
```erlang
%% Стартираме scheduler-а
{ok, _} = raptor_scheduler:start_link(),

%% Дефинираме списък от services
Services = ["crawler", "mobile_de", "raptor"],

%% Стартираме тях последователно (веднъж)
ok = raptor_scheduler:schedule_services(Services),

%% Проверяваме статуса
Status = raptor_scheduler:get_status(),
io:format("Current: ~p~n", [maps:get(current_service, Status)]).
```

### Пример 2: Периодично (на всеки 24 часа)
```erlang
%% Стартираме scheduler-а
{ok, _} = raptor_scheduler:start_link(),

%% Дефинираме списък от services
Services = ["crawler", "mobile_de", "raptor"],

%% Настройваме periodic стартиране - на всеки 24 часа
IntervalMs = 24 * 60 * 60 * 1000,
ok = raptor_scheduler:schedule_periodic(Services, IntervalMs),

%% Scheduler-ът ще стартира services:
%% - Веднага при настройка
%% - След 24 часа след приключване на първия цикъл
%% - След следващите 24 часа и т.н.

%% Можем да спрем periodic стартирането
ok = raptor_scheduler:cancel_periodic().
```

### Пример 3: Периодично (на всеки 1 час - за тестване)
```erlang
{ok, _} = raptor_scheduler:start_link(),
Services = ["test_service"],
IntervalMs = 60 * 60 * 1000,  %% 1 час
ok = raptor_scheduler:schedule_periodic(Services, IntervalMs).
```

## Slack нотификации

Scheduler-ът праща **две вида** Slack съобщения:

### 1. За всеки service (от raptor_service_fsm)
- ✅ Service completed successfully
- ⚠️ Service failed (attempt X) - на всеки retry
- ❌ Service failed permanently after N attempts

### 2. Обобщение в края (от scheduler-а)
```
📊 Scheduler Summary
Total: 3 | Success: 2 | Failed: 1
Duration: 125.3 seconds

✅ service1
✅ service2
❌ service3
```

## Резултати

Резултатите се записват във формат:
```erlang
{ServiceName, Status, Info}
```

Където:
- `ServiceName` - string, името на service-а
- `Status` - `ok` | `error`
- `Info` - `success` | error reason

Пример:
```erlang
[
    {"service1", ok, success},
    {"service2", ok, success},
    {"service3", error, {timeout, ...}}
]
```

## Интеграция с raptor_service_fsm

Scheduler-ът използва `raptor_service_fsm` за всеки service:
- Стартира FSM чрез `raptor_service_fsm:start_link(ServiceName)`
- Поставя monitor с `erlang:monitor(process, Pid)`
- Получава `{'DOWN', ...}` съобщение когато FSM терминира
- Проверява причината (normal = успех, {shutdown, Reason} = грешка)

## Обработка на грешки

Ако FSM-а не може да се стартира:
```erlang
{ServiceName, error, {failed_to_start_fsm, Reason}}
```

Scheduler-ът **продължава** със следващия service дори при грешка.
