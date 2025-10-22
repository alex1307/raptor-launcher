# 🧠 Raptor Launcher – System Architecture

## Overview

**Raptor Launcher** is an Erlang/OTP-based orchestrator responsible for managing and supervising the full Raptor ecosystem — including Docker services, Kafka infrastructure, Chrome headless automation, and all Raptor crawlers and UI services.

The system provides a **production-grade control layer** for distributed components.  
It enables unified startup, monitoring, restart, and status tracking across macOS and Ubuntu, driven entirely by YAML and environment configuration.

---

## 🏗️ Architecture Overview

```
apps/
├── main_app/         # Central supervisor and orchestrator
├── common_lib/       # Shared utilities (cmd_utils, yml_utils, env expansion)
├── docker_app/       # Docker daemon and Compose management
├── kafka/            # Kafka topic configuration and monitoring
├── chrome_app/       # Chrome lifecycle control (start, stop, health)
└── raptors/          # Raptor crawlers & frontend orchestration
```

Each app is self-contained with:

- `*_app.erl` – defines application start and supervision tree
- `*_sup.erl` – supervises one or more `*_srv` workers
- `*_srv.erl` – executes actual logic (start/stop/status)
- `*_utils.erl` – handles YAML parsing and command templating

All modules read a **single configuration file**:

```
devops/launcher.yml
```

which is enriched at runtime with `.env` values from:

```
devops/env/raptor.env
```

---

## ⚙️ Core Modules

### 🐳 docker_app

- Controls Docker daemon and Compose stack (`start`, `stop`, `restart`).
- Supports OS-specific commands (macOS / Linux).
- Tracks container health via `docker inspect`.
- Reads `compose_workdir` from environment (`DOCKER_COMPOSE_WORKDIR`).

### 📡 kafka_app

- Runs inside Docker context (`kafka-server` container).
- Creates and alters topics (`raptor.mobile_de`, `raptor.metadata`).
- Reads retention policies from YAML:

  ```yaml
  kafka.configs:
    retention.ms: 86400000
    retention.bytes: 104857600
  ```

- All commands templated dynamically and executed via `cmd_utils`.

### 🌐 chrome_app

- Manages headless Chrome lifecycle.
- On Linux → starts `Xvfb :0` and Chrome with `--remote-debugging-port=9223`.
- On macOS → launches native Chrome binary.
- Checks health by calling `http://127.0.0.1:9223/json/version`.
- Can detect and gracefully stop Chrome by port.

### 🦅 raptors_app

- Supervises all Raptor crawlers (`autouncle.*`, `mobile_de`) and UI service.
- Reads service map from YAML (`raptor-services`):

  ```yaml
  raptor-services:
    crawler-de:
      start_script: "devops/bin/start-crawler.sh autouncle.de"
      stop_script: "devops/bin/stop-crawler.sh"
    mobile_de:
      start_script: "devops/bin/start-mobile_de.sh"
      stop_script: "devops/bin/stop-mobile_de.sh"
  ```

- Provides unified operations:
  - `start_service(Name)`
  - `stop_service(Name)`
  - `is_service_running(Name)`
  - `list_services()`

---

## 🧰 Shared Libraries

### `cmd_utils.erl`

- Executes system commands with timeout.
- Captures stdout/stderr.
- Provides `{ok, Output}` | `{error, Reason}` return types.
- Handles encoding and command safety across OS types.

### `yml_utils.erl`

- Parses YAML configs using `yamerl`.
- Supports `${VAR}` expansion via `os:getenv/1`.
- Returns normalized maps for all apps.

---

## 🔄 Boot Process

1. **main_app** starts as root OTP application.
2. Loads YAML + environment configuration.
3. Sequentially starts:
   - `docker_app`
   - `kafka`
   - `chrome_app`
   - `raptors`
4. Each service logs initialization via `lager` and updates runtime status.

---

## 🪶 Design Principles

- **Modular** – each service is independent, OTP-supervised.
- **Config-driven** – YAML defines all behavior; no hardcoded paths.
- **Cross-platform** – works equally on macOS and Ubuntu.
- **Fault-tolerant** – OTP supervision ensures auto-restart on failure.
- **Lightweight** – idle memory per service < 1 MB.

---

## 🧠 Future Roadmap

- Scheduler for hourly health checks and auto-restarts.
- Slack integration for system notifications.
- Metrics collector for uptime and resource usage.
- REST API layer for external orchestration.
- Distributed clustering between multiple hosts.

---

## 📦 Summary

Raptor Launcher transforms infrastructure control into a declarative, fault-tolerant Erlang system.

It’s not a collection of scripts — it’s an **autonomous orchestration engine** capable of starting, supervising, and healing an entire distributed data stack with minimal overhead.
