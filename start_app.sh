#!/bin/bash

set -a; \
source /home/matkat/launcher-app/devops/env/raptor.systemd.env; \
source /home/matkat/launcher-app/devops/env/.systemd.env; \
set +a; \
exec ./raptor_launcher/bin/raptor_launcher foreground