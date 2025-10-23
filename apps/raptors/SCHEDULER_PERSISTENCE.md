# Scheduler Persistence (Crash Recovery)

## Как работи?

Scheduler-ът използва DETS (disk-based ETS) за да запази своя state и да се възстанови след crash.

### DETS Таблица

- **Име на таблицата**: `scheduler_state` (обща за всички schedulers)
- **Файл**: `/tmp/scheduler_state.dets`
- **Формат**: `{SchedulerName, StateMap}`

Всеки scheduler записва своя state под своето име като ключ.

### State Format

```erlang
StateMap = #{
    services_queue => ["service1", "service2", ...],
    current_service => "service_name" | undefined,
    results => [{ServiceName, Status, Info}, ...]
}
```

### Кога се запазва state?

State се запазва в DETS след всяка промяна:

1. **След стартиране на нов service** - запазва опашката и текущия service
2. **След приключване на service** - запазва резултатите

### Кога се ИЗТРИВА state?

State се изтрива когато:

1. **Всички services приключат** - нормално завършване на цикъла
2. **Scheduler се терминира нормално без незавършени services**

### Кога се ЗАПАЗВА state при терминация?

State се запазва при терминация само ако:

1. **Има незавършени services в опашката** - crash recovery
2. **Има текущ service който се изпълнява**

## Пример: Crash Recovery

```erlang
%% 1. Стартираме scheduler с 5 services
{ok, Pid} = raptor_scheduler:start_link(my_scheduler, 60000),
raptor_scheduler:schedule_services(my_scheduler, ["s1", "s2", "s3", "s4", "s5"]),

%% 2. След 8 секунди scheduler-ът ще е стартирал 2-3 services
timer:sleep(8000),

%% 3. Crash - убиваме процеса
exit(Pid, kill),

%% 4. Рестартираме scheduler-а с СЪЩОТО ИМЕ
{ok, NewPid} = raptor_scheduler:start_link(my_scheduler, 60000),

%% 5. Scheduler-ът автоматично възстановява state:
%%    - Зарежда запазената опашка
%%    - Продължава от където е спрял
%%    - НЕ стартира отново завършените services
```

## Важни моменти

1. **Едно име = един state** - Ако рестартирате scheduler с различно име, той няма да възстанови state-а
2. **State се споделя в една DETS таблица** - Всички schedulers използват `/tmp/scheduler_state.dets`
3. **Автоматично изчистване** - След като cycle приключи, state-ът се изтрива автоматично
4. **Periodic scheduling** - При periodic schedulers, state-ът се изтрива след всеки цикъл и periodic cycle-ът продължава нормално

## Проверка на запазен state

```erlang
%% Отваряне на DETS таблицата
{ok, Table} = dets:open_file(scheduler_state, [{file, "/tmp/scheduler_state.dets"}, {type, set}]),

%% Преглед на всички записи
dets:match(Table, '$1').

%% Проверка за конкретен scheduler
dets:lookup(Table, my_scheduler).

%% Затваряне
dets:close(Table).
```

## Тестване

Виж примера в `raptor_scheduler_crash_recovery_example.erl`:

```erlang
%% Кратък тест
raptor_scheduler_crash_recovery_example:crash_demo().

%% Пълна демонстрация
raptor_scheduler_crash_recovery_example:demo().
```
