# Auto-Update Setup for Raptor Launcher

Автоматично актуализира raptor-launcher при нови Git commits.

## Quick Setup (Препоръчан начин)

На Ubuntu сървъра, след като си deploy-нал `launcher-app`:

```bash
cd ~/launcher-app/devops
./setup-auto-update.sh
```

Този интерактивен скрипт ще:

- ✅ Конфигурира cron job
- ✅ Настрои sudo permissions
- ✅ Setup-не Slack notifications (optional)
- ✅ Тества че всичко работи

---

## Manual Setup (Ако искаш ръчно)

### 1. Провери че скриптът работи ръчно

```bash
cd ~/launcher-app/devops
./auto-update.sh
```

### 2. Настрой Cron Job

```bash
# Редактирай crontab:
crontab -e

# Добави един от следните редове:

# Провери на всеки 10 минути:
*/10 * * * * /home/matkat/launcher-app/devops/auto-update.sh >> /home/matkat/raptor-auto-update.log 2>&1

# Или на всеки 30 минути (със Slack):
*/30 * * * * SLACK_WEBHOOK_URL="https://hooks.slack.com/..." /home/matkat/launcher-app/devops/auto-update.sh >> /home/matkat/raptor-auto-update.log 2>&1
```

### 3. Провери дали работи

```bash
# Виж логовете:
tail -f ~/raptor-auto-update.log

# Или провери cron логовете:
grep CRON /var/log/syslog | tail -20
```

## Какво прави скриптът?

1. ✅ **Fetch** - Проверява за нови commits в Git
2. ✅ **Compare** - Сравнява local с remote
3. ✅ **Notify** - Изпраща Slack notification че започва update
4. ✅ **Pull** - Тегли новия код
5. ✅ **Stop** - Спира raptor-launcher service
6. ✅ **Backup** - Прави backup на текущия deployment
7. ✅ **Build** - Прави `make deploy`
8. ✅ **Restore Config** - Копира env файловете от backup
9. ✅ **Deploy** - Reinstall-ва systemd service
10. ✅ **Start** - Стартира service-а отново
11. ✅ **Health Check** - Проверява че service-ът работи
12. ✅ **Notify** - Изпраща Slack notification с резултат
13. ✅ **Cleanup** - Трие стари backups (пази последните 3)

## Slack Notifications

За да получаваш Slack notifications, добави `SLACK_WEBHOOK_URL` в environment:

```bash
# В ~/.bashrc или в crontab:
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
```

Или добави директно в crontab:

```bash
*/10 * * * * SLACK_WEBHOOK_URL="https://hooks.slack.com/..." /home/matkat/raptor-launcher/devops/auto-update.sh >> /home/matkat/raptor-auto-update.log 2>&1
```

## Rollback при проблем

Ако update гръмне, скриптът автоматично:

1. Restore-ва backup-а
2. Стартира старата версия
3. Изпраща error notification в Slack

За ръчен rollback:

```bash
cd ~/raptor-launcher
sudo systemctl stop raptor-launcher

# Виж кои backups има:
ls -lah launcher-app.backup.*

# Restore последния backup:
rm -rf launcher-app
mv launcher-app.backup.20241026_103045 launcher-app

sudo systemctl start raptor-launcher
```

## Manual Trigger

Ако искаш да trigger-неш update ръчно:

```bash
cd ~/raptor-launcher/devops
./auto-update.sh
```

## Troubleshooting

### Скриптът не се изпълнява

```bash
# Провери дали е executable:
ls -la ~/raptor-launcher/devops/auto-update.sh

# Направи го executable:
chmod +x ~/raptor-launcher/devops/auto-update.sh
```

### Няма Slack notifications

```bash
# Провери дали SLACK_WEBHOOK_URL е сетната:
echo $SLACK_WEBHOOK_URL

# Тествай ръчно:
curl -X POST -H 'Content-type: application/json' \
     --data '{"text":"Test message"}' \
     "$SLACK_WEBHOOK_URL"
```

### Build-ът гърми

```bash
# Виж build логовете:
cat /tmp/raptor-build.log

# Ръчно build:
cd ~/raptor-launcher
make deploy
```

## Permissions

Скриптът използва `sudo` за systemctl команди. Добави в `/etc/sudoers.d/raptor`:

```bash
# Създай файла:
sudo visudo -f /etc/sudoers.d/raptor

# Добави:
matkat ALL=(ALL) NOPASSWD: /bin/systemctl start raptor-launcher
matkat ALL=(ALL) NOPASSWD: /bin/systemctl stop raptor-launcher
matkat ALL=(ALL) NOPASSWD: /bin/systemctl restart raptor-launcher
matkat ALL=(ALL) NOPASSWD: /home/matkat/launcher-app/devops/install-service.sh
```

## Advanced: Webhook вместо Cron

Ако искаш instant updates (без да чакаш cron), виж [WEBHOOK_SETUP.md](WEBHOOK_SETUP.md).
