set -a; \
source devops/env/raptor.systemd.env; \
source devops/env/.systemd.env; \
set +a; \
/home/matkat/launcher-app/bin/raptor_launcher foreground