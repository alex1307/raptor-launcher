launcher
=====

Purpose
-------

The launcher application orchestrates system resources such as Docker, Postgres, Kafka, and Chrome before starting higher-level services. It ensures that all dependencies are properly initialized and ready for operation.

Architecture
------------

Launcher is designed as two finite state machines (FSMs): the `resource_orchestrator`, which manages the initialization and readiness of system resources, and the upcoming `service_orchestrator` for managing higher-level services. Both FSMs are supported by shared utilities located in the `common_lib`.

```
[Resource FSM]
  idle
   ↓
  starting_docker
   ↓
  starting_postgres
   ↓
  starting_kafka
   ↓
  starting_chrome
   ↓
  ready → cast(service_orchestrator, {resources_ready})

[Service FSM]
  waiting_resources
   ↓
  starting_services
   ↓
  running
   ↓
  monitoring (periodic checks)
```

States
------

Launcher progresses through the following states in order:

- network  
- docker  
- postgres  
- kafka  
- chrome  
- ready  
- monitoring

Each state represents the readiness of a specific component or phase in the startup sequence.

Monitoring & Recovery
---------------------

Launcher performs periodic health checks on all managed components. It sends notifications via Slack for any detected issues and attempts automatic restarts of failed components to maintain system stability.

Configuration
-------------

Launcher is configured via a YAML file (`launcher.yml`) for resource definitions and settings. Slack webhook URLs for notifications can be provided through environment variables or included in the YAML configuration.

Future
------

A service orchestration layer is planned to manage services such as `mobile_de`, `crawler`, and `raptor`, further extending launcher's capabilities beyond resource management.
