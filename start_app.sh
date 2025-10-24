set -a; \
source devops/env/raptor.systemd.env; \
source devops/env/.systemd.env; \
set +a; \
./bin/raptor_launcher foreground