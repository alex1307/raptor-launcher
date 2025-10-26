# Environment Files

Този проект използва различни environment файлове в зависимост от контекста.

## Файлове

### 1. `raptor.env` (Bash Script)
**Използва се от:** Makefile (`make run-dev`, `make run-prod`)

**Формат:** Bash script с `export`, `if/else`, command substitution

**Пример:**
```bash
export HOME="${HOME:-$(eval echo ~)}"

if [ "$(uname)" = "Darwin" ]; then
  export DOCKER_COMPOSE_WORKDIR="$HOME/Software/docker-env"
else
  export DOCKER_COMPOSE_WORKDIR="/srv/docker"
fi

export DATABASE_URL="postgres://admin:1234@localhost:5432/vehicles"
```

**Предимства:**
- Динамично определя пътища според OS (macOS vs Linux)
- Може да използва shell логика
- Работи отлично за interactive shell (`rebar3 shell`)

---

### 2. `raptor.systemd.env` (Systemd EnvironmentFile)
**Използва се от:** systemd service (`/etc/systemd/system/raptor-launcher.service`)

**Формат:** САМО прости `KEY=VALUE` двойки, **БЕЗ** bash syntax

**Пример:**
```bash
HOME=/home/matkat
DOCKER_COMPOSE_WORKDIR=/srv/docker
DATABASE_URL=postgres://admin:1234@localhost:5432/vehicles
KAFKA_BROKER=localhost:9094
```

**Ограничения (systemd НЕ поддържа):**
- ❌ `export` команди
- ❌ `if/else` логика
- ❌ Command substitution `$(...)`
- ❌ Shell variables `${VAR}`
- ❌ Bash функции

**Важно:** Пътищата трябва да са **hardcoded** за конкретната система!

---

### 3. `.env` (Secrets - Bash compatible)
**Използва се от:** Makefile (`make run-dev`, `make run-prod`)

**Формат:** Прости `KEY=VALUE` двойки за secrets

**Пример:**
```bash
COMPOSE_PROJECT_NAME=raptor
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/T09.../B09.../9xAZ...
```

**Забележка:** Този файл **НЕ** трябва да се commit-ва в git (добавен в `.gitignore`)

---

### 4. `.systemd.env` (Secrets - Systemd compatible)
**Използва се от:** systemd service (`/etc/systemd/system/raptor-launcher.service`)

**Формат:** Прости `KEY=VALUE` двойки за secrets (идентични с `.env`)

**Пример:**
```bash
COMPOSE_PROJECT_NAME=raptor
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/T09.../B09.../9xAZ...
```

**Забележка:** 
- Този файл **НЕ** трябва да се commit-ва в git (покрит от `*env` в `.gitignore`)
- Използвай `.systemd.env.example` като template

---

## Кога да използваш кой файл?

### Development (macOS или Linux)
```bash
make run-dev
# Използва: raptor.env + .env
# Работи с bash логика, динамични пътища
```

### Production (systemd на Ubuntu)
```bash
sudo systemctl start raptor-launcher
# Използва: raptor.systemd.env + .env
# Изисква статични пътища за Ubuntu
```

---

## Migration Guide: raptor.env → raptor.systemd.env

Ако трябва да създадеш `raptor.systemd.env` от съществуващ `raptor.env`:

1. **Премахни `export`:**
   ```bash
   # ПРЕДИ
   export DATABASE_URL="postgres://..."
   
   # СЛЕД
   DATABASE_URL=postgres://...
   ```

2. **Hardcode пътищата за Ubuntu:**
   ```bash
   # ПРЕДИ
   if [ "$(uname)" = "Darwin" ]; then
     export DOCKER_COMPOSE_WORKDIR="$HOME/Software/docker-env"
   else
     export DOCKER_COMPOSE_WORKDIR="/srv/docker"
   fi
   
   # СЛЕД (само Linux variant)
   DOCKER_COMPOSE_WORKDIR=/srv/docker
   ```

3. **Премахни command substitution:**
   ```bash
   # ПРЕДИ
   export HOME="${HOME:-$(eval echo ~)}"
   
   # СЛЕД
   HOME=/home/matkat
   ```

4. **Премахни всички `if/else`, loops, функции**

---

## Troubleshooting

### Systemd игнорира environment variables

**Симптом:**
```
systemd[1]: raptor-launcher.service: Ignoring invalid environment assignment 'export HOME=...'
```

**Причина:** Използваш `raptor.env` (bash script) вместо `raptor.systemd.env`

**Решение:** 
```bash
# В raptor-launcher.service промени:
EnvironmentFile=/home/matkat/raptor-launcher/devops/env/raptor.systemd.env
```

### Makefile не зарежда variables

**Симптом:** `DATABASE_URL` е празна когато стартираш с `make run-dev`

**Причина:** Makefile трябва да използва bash shell

**Решение:** Провери че Makefile има:
```makefile
SHELL := /bin/bash
```

---

## Проверка дали variables са заредени

### От Makefile context:
```bash
make run-dev
# В erlang shell:
os:getenv("DATABASE_URL").
```

### От systemd context:
```bash
sudo systemctl show raptor-launcher --property=Environment
```

---

## Best Practices

1. **Винаги използвай правилния env файл за контекста**
   - Interactive shell → `raptor.env`
   - Systemd service → `raptor.systemd.env`

2. **Пази secrets в `.env`**
   - Slack webhooks
   - API keys
   - Passwords

3. **НЕ commit-вай `.env` в git**
   - Добави в `.gitignore`
   - Използвай `.env.example` за template

4. **Тествай и двата контекста**
   - `make run-dev` (development)
   - `sudo systemctl start raptor-launcher` (production)
