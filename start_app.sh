#!/bin/bash

cd "$(dirname "$0")"
set -a;
source devops/env/raptor.systemd.env
source devops/env/.systemd.env
set +a; \
exec ./bin/raptor_launcher foreground