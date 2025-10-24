# Systemd Service Deployment (Ubuntu)

Ръководство за deploy на `raptor-launcher` като systemd сървиз на Ubuntu.

## Предварителни изисквания

1. **Erlang/OTP** инсталиран (версия 28 или по-нова)
2. **rebar3** инсталиран
3. **PostgreSQL** running (за DATABASE_URL)
4. **Docker** running (за crawler сървизите)

## Deployment Steps

### 1. Build Production Release

```bash
cd ~/raptor-launcher

# Clean build
rebar3 clean

# Compile production release
make release-prod

# Проверка - release трябва да е създаден тук:
ls -la _build/default/rel/raptor_launcher/bin/raptor_launcher
```

### 2. Конфигурация на environment файловете

### 2.1. Копирай и редактирай `raptor.systemd.env`

```bash
cd ~/raptor-launcher/devops/env
nano raptor.systemd.env
```

Настрой пътищата за Ubuntu:

```bash
HOME=/home/matkat
DOCKER_COMPOSE_WORKDIR=/home/matkat/raptor-launcher/devops
DATABASE_URL=postgresql://username:password@localhost:5432/dbname
KAFKA_BROKER=localhost:9092
# ... други настройки
```

### 2.2. Създай `.systemd.env` файл (secrets)

```bash
cd ~/raptor-launcher/devops/env
cp .systemd.env.example .systemd.env
nano .systemd.env
```

Добави тайните стойности:

```bash
COMPOSE_PROJECT_NAME=raptor
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/YOUR_WEBHOOK_URL
```

**Важно:** Файлът `.systemd.env` не трябва да се commit-ва в git (покрит от `*env` в `.gitignore`)

---

### 3. Install Systemd Service

```bash
cd ~/raptor-launcher

# Install сървиза (изисква sudo)
sudo ./devops/install-service.sh
```

Скриптът ще:

- Копира `raptor-launcher.service` в `/etc/systemd/system/`
- Reload systemd daemon
- Enable сървиза да стартира при boot

### 4. Start Service

```bash
# Стартирай сървиза
sudo systemctl start raptor-launcher

# Провери статуса
sudo systemctl status raptor-launcher

# Виж логовете (real-time)
sudo journalctl -u raptor-launcher -f
```

## Service Management

### Основни команди

```bash
# Start
sudo systemctl start raptor-launcher

# Stop
sudo systemctl stop raptor-launcher

# Restart
sudo systemctl restart raptor-launcher

# Status
sudo systemctl status raptor-launcher

# Enable auto-start при boot
sudo systemctl enable raptor-launcher

# Disable auto-start при boot
sudo systemctl disable raptor-launcher
```

### Логове

```bash
# Real-time logs (follow)
sudo journalctl -u raptor-launcher -f

# Last 100 lines
sudo journalctl -u raptor-launcher -n 100

# Today's logs
sudo journalctl -u raptor-launcher --since today

# Logs between timestamps
sudo journalctl -u raptor-launcher --since "2025-01-20 10:00" --until "2025-01-20 12:00"

# Show only errors
sudo journalctl -u raptor-launcher -p err
```

## Troubleshooting

### Service не стартира

1. **Провери дали release е build-нат:**

```bash
ls -la ~/raptor-launcher/_build/default/rel/raptor_launcher/bin/raptor_launcher
```

Ако липсва:

```bash
cd ~/raptor-launcher
make release-prod
```

2. **Провери логовете за грешки:**

```bash
sudo journalctl -u raptor-launcher -n 50
```

3. **Провери дали PostgreSQL и Docker са running:**

```bash
sudo systemctl status postgresql
sudo systemctl status docker
```

4. **Тествай ръчно release-а:**

```bash
cd ~/raptor-launcher
_build/default/rel/raptor_launcher/bin/raptor_launcher console
```

### Environment Variables не се зареждат

Провери дали файловете съществуват:

```bash
ls -la ~/raptor-launcher/devops/env/raptor.env
ls -la ~/raptor-launcher/devops/env/.env
```

Тествай дали се зареждат:

```bash
# От running сървиз
sudo systemctl show raptor-launcher --property=Environment
```

### Permission errors

Увери се че `matkat` user има достъп до всички файлове:

```bash
sudo chown -R matkat:matkat ~/raptor-launcher
chmod +x ~/raptor-launcher/_build/default/rel/raptor_launcher/bin/raptor_launcher
```

### Service crash-ва след старт

1. **Провери Erlang crash dumps:**

```bash
cat ~/raptor-launcher/erl_crash.dump
```

2. **Провери application логове:**

```bash
tail -f ~/raptor-launcher/log/log/console.log.0
tail -f ~/raptor-launcher/log/log/error.log.0
```

3. **Increase restart delay в service файла:**

```bash
sudo nano /etc/systemd/system/raptor-launcher.service
# Промени: RestartSec=30
sudo systemctl daemon-reload
sudo systemctl restart raptor-launcher
```

## Uninstall Service

```bash
cd ~/raptor-launcher
sudo ./devops/uninstall-service.sh
```

Или ръчно:

```bash
sudo systemctl stop raptor-launcher
sudo systemctl disable raptor-launcher
sudo rm /etc/systemd/system/raptor-launcher.service
sudo systemctl daemon-reload
```

## Service File Location

- **Source:** `~/raptor-launcher/devops/raptor-launcher.service`
- **Installed:** `/etc/systemd/system/raptor-launcher.service`

## Key Configuration

```ini
[Service]
Type=simple
User=matkat
WorkingDirectory=/home/matkat/raptor-launcher
EnvironmentFile=/home/matkat/raptor-launcher/devops/env/raptor.env
EnvironmentFile=-/home/matkat/raptor-launcher/devops/env/.env
ExecStart=/home/matkat/raptor-launcher/_build/default/rel/raptor_launcher/bin/raptor_launcher foreground
Restart=on-failure
RestartSec=10
```

## Production Checklist

- [ ] Build production release: `make release-prod`
- [ ] Configure `devops/env/raptor.env` (Ubuntu paths)
- [ ] Configure `devops/env/.env` (Slack webhook, secrets)
- [ ] PostgreSQL running and accessible
- [ ] Docker running
- [ ] Install service: `sudo ./devops/install-service.sh`
- [ ] Start service: `sudo systemctl start raptor-launcher`
- [ ] Check logs: `sudo journalctl -u raptor-launcher -f`
- [ ] Verify schedulers: Check logs for service starts
- [ ] Enable auto-start: `sudo systemctl enable raptor-launcher`

## Development vs Production

### Development (interactive shell)

```bash
make run-dev
# или
make run-prod
```

### Production (systemd service)

```bash
make release-prod
sudo systemctl start raptor-launcher
```

**Защо не `make run-prod` в systemd?**

- `make run-prod` стартира **interactive Erlang shell** (`rebar3 shell`)
- Systemd изисква **foreground daemon** (non-interactive)
- Release mode (`raptor_launcher foreground`) е правилният начин за production deployment

## Auto-start on Boot

Сървизът е конфигуриран да стартира автоматично при boot с:

```bash
sudo systemctl enable raptor-launcher
```

За да провериш дали е enabled:

```bash
systemctl is-enabled raptor-launcher
```

## Scheduler Status from Logs

```bash
# Провери кои сървизи се стартират
sudo journalctl -u raptor-launcher | grep "starting service"

# Провери scheduler статус
sudo journalctl -u raptor-launcher | grep "raptor_scheduler"

# Провери за грешки
sudo journalctl -u raptor-launcher -p err
```
