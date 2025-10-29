# Deployment Guide

## На macOS (development)

```bash
# Компилиране и пускане:
make run-dev

# Или production конфигурация:
make run-prod
```

## На Ubuntu (production с systemd)

### 1. Подготовка на deployment пакета (на macOS)

```bash
cd ~/raptor-launcher
make deploy
```

Това ще създаде `launcher-app/` директория с цялата нужна структура:
```
launcher-app/
├── raptor_launcher/          # Erlang release
│   ├── bin/
│   │   └── raptor_launcher   # Startup script
│   ├── releases/
│   ├── lib/
│   └── erts-*/
├── devops/                   # Configuration & scripts
│   ├── env/
│   │   ├── raptor.systemd.env       # Config (commit to git)
│   │   ├── .systemd.env.example     # Secrets template
│   │   └── README.md
│   ├── raptor-launcher.service
│   ├── install-service.sh
│   ├── uninstall-service.sh
│   └── launcher.yml
└── bin/                      # Additional scripts
    └── start_app.sh
```

### 2. Copy на Ubuntu сървъра

```bash
# Архивирай (на macOS):
cd ~/raptor-launcher
tar czf launcher-app.tar.gz launcher-app/

# Копирай на Ubuntu:
scp launcher-app.tar.gz matkat@ubuntu-server:~/

# На Ubuntu сървъра:
cd ~
tar xzf launcher-app.tar.gz
mv launcher-app raptor-launcher/launcher-app
```

### 3. Конфигурация на Ubuntu

```bash
cd ~/raptor-launcher/launcher-app/devops/env

# Редактирай configuration:
nano raptor.systemd.env
# Провери пътищата, DATABASE_URL, KAFKA_BROKER, etc.

# Създай secrets файла:
cp .systemd.env.example .systemd.env
nano .systemd.env
# Добави SLACK_WEBHOOK_URL и други secrets
```

### 4. Инсталиране на systemd service

```bash
cd ~/raptor-launcher/launcher-app/devops
sudo ./install-service.sh
```

### 5. Стартиране

```bash
# Стартирай service:
sudo systemctl start raptor-launcher

# Провери статус:
sudo systemctl status raptor-launcher

# Гледай логове:
sudo journalctl -u raptor-launcher -f

# Рестартирай при нужда:
sudo systemctl restart raptor-launcher
```

### 6. (Optional) Setup Auto-Update

За автоматични updates при git push:

```bash
cd ~/launcher-app/devops
./setup-auto-update.sh
```

Виж [AUTO_UPDATE.md](devops/AUTO_UPDATE.md) за детайли.

---

## Актуализация (Update)

### Автоматично (Препоръчано)

След като си setup-нал auto-update (стъпка 6), просто:

```bash
# На macOS:
git push

# На Ubuntu: автоматично ще се обнови след максимум 10 минути
```

### Ръчно

#### На macOS:

```bash
cd ~/raptor-launcher
git pull
make deploy
tar czf launcher-app.tar.gz launcher-app/
scp launcher-app.tar.gz matkat@ubuntu-server:~/
```

### На Ubuntu:

```bash
# Спри service:
sudo systemctl stop raptor-launcher

# Backup на старата версия:
cd ~/raptor-launcher
mv launcher-app launcher-app.backup.$(date +%Y%m%d)

# Разархивирай новата:
tar xzf ~/launcher-app.tar.gz

# Копирай environment файловете от backup:
cp launcher-app.backup.*/devops/env/.systemd.env launcher-app/devops/env/

# Стартирай отново:
sudo systemctl start raptor-launcher
sudo journalctl -u raptor-launcher -f
```

## Troubleshooting

### Service не се стартира

```bash
# Провери статус:
sudo systemctl status raptor-launcher

# Провери логове:
sudo journalctl -u raptor-launcher -n 100

# Тествай ръчно:
cd ~/raptor-launcher/launcher-app/raptor_launcher
./bin/raptor_launcher foreground
```

### Environment променливи не се зареждат

```bash
# Провери файловете:
cat ~/raptor-launcher/launcher-app/devops/env/raptor.systemd.env
cat ~/raptor-launcher/launcher-app/devops/env/.systemd.env

# Презареди service:
sudo systemctl daemon-reload
sudo systemctl restart raptor-launcher
```

### Kafka/PostgreSQL/Docker проблеми

```bash
# Провери услугите:
sudo systemctl status postgresql
sudo systemctl status docker
docker ps

# Рестартирай всичко:
sudo systemctl restart docker
sudo systemctl restart postgresql
sudo systemctl restart raptor-launcher
```

## Deinstallation

```bash
cd ~/raptor-launcher/launcher-app/devops
sudo ./uninstall-service.sh
```
