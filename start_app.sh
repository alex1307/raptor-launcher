#!/bin/bash
set -a
source devops/env/raptor-local.env
set +a
rebar3 shell


set -a
source devops/env/raptor-local.env
set +a
rebar3 eunit --module=chrome_utils_test