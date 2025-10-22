# 🦅 Raptor Launcher 2.0

Raptor Launcher is an Erlang/OTP-based orchestrator designed to manage, start, and monitor distributed components such as:

- Docker infrastructure (Kafka, Postgres, UI)
- Kafka brokers and topics
- Chrome headless processes
- Raptor crawler and backend services

Its purpose is to provide a **single, fault-tolerant entry point** for the full stack — stable, modular, and fully configurable for both macOS and Ubuntu.

---

## 🧩 Architecture

Raptor Launcher follows a modular OTP structure:

```
apps/
├── common_lib/      # Shared utilities (YAML parsing, command execution)
├── docker_app/      # Manages Docker, Compose, and container lifecycle
├── kafka/           # Creates/configures Kafka topics and retention policies
├── chrome_app/      # Controls Chrome start/stop/status with X-server
└── main_app/        # Main orchestrator & supervisor tree
```

Each module:

- Has its own supervisor (`*_sup.erl`) and service (`*_srv.erl`)
- Loads configuration dynamically from YAML
- Logs via `lager`
- Uses `cmd_utils` for safe command execution with timeouts

This design ensures modularity and fault isolation — if one service fails, the rest continue unaffected.

---

## ⚙️ Configuration

Configuration is defined in a single YAML file:

```
devops/launcher.yml
```

Example (excerpt):

```yaml
docker:
  enabled: true
  compose_workdir: "~/Software/docker-env"

  commands:
    is_alive: "docker info > /dev/null 2>&1 && echo ok || echo error"
    start_compose: "cd {{workdir}} && docker compose up -d"
    stop_compose: "cd {{workdir}} && docker compose down"

kafka:
  container: "kafka-server"
  bootstrap_server: "localhost:9092"
  topics:
    mobile_de: "raptor.mobile_de"
    metadata: "raptor.metadata"

chrome:
  enabled: true
  port: 9223
  start_script: "devops/bin/start-chrome.sh"
  stop_cmd: "pkill -f remote-debugging-port=9223 || true"
```

All modules read their configuration via `yml_utils:yml2map/1` from this file.

---

## 🧰 Environment

`.env` files are used to configure environment-specific settings (paths, display, Slack hooks, etc.):

Example: `devops/env/raptor-local.env`

```bash
export DISPLAY=":0"
export HOME="${HOME:-$(eval echo ~)}"
export XAUTHORITY="$HOME/.Xauthority"

export CHROME_BIN_MAC="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
export CHROME_BIN_UBUNTU="/usr/bin/google-chrome"
```

These variables are automatically loaded when starting the system (see Makefile).

---

## 🏗️ Setup

### Prerequisites

- Erlang/OTP 25+ (recommended 26+)
- rebar3
- Docker & Docker Compose
- Xvfb (for headless Chrome on Ubuntu)
- Git

### Build & Run

```bash
make build
make run
```

`make run` automatically loads environment variables and starts `rebar3 shell`.

---

## 🧠 Design Highlights

- **Cross-platform orchestration** (macOS + Ubuntu)
- **Unified YAML config** for Docker, Kafka, Chrome, and Raptor services
- **OTP supervision tree** – each service runs independently
- **Centralized logging** via `lager`
- **Extensible architecture** – easy to add new modules (e.g., slack_app, monitor_app)
- **Near-zero idle CPU usage** (~1 MB per process)

---

## 🧩 Roadmap

- [ ] Add scheduler/monitor for periodic health checks
- [ ] Integrate Slack/Teams notifications
- [ ] REST API for service management
- [ ] Self-healing restart logic

---

## 🧾 License

MIT License © 2025 Raptor Systems