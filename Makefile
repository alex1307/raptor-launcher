# Force bash shell (required for 'source' command on Ubuntu)
SHELL := /bin/bash

CUR_DIR = $(CURDIR)

fix:
	rebar3 clean
	rm -rf _build
	rm -rf log
	rebar3 get-deps
# 	cp -f $(CUR_DIR)/config/jwerl_hs.erl _build/default/lib/jwerl/src

all:
	rebar3 clean compile

clean:
	rebar3 clean
	rm -rf _build
	rm -rf log
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

release-dev:
	rebar3 clean compile
# 	rebar3 eunit
	rebar3 release

release: release-prod

release-prod:
	rebar3 clean compile
	rebar3 as prod release

#

# blank:
# 	cd rel && rm -rf workernode*; cd ..
# 	./rebar delete-deps clean

# ct:
# 	rm -rf apps/*/logs/
# 	./rebar ct skip_deps=true

# ct_case:
# 	rm -rf apps/*/logs/
# 	./rebar ct skip_deps=true suites=${SUITE} case=${CASE}

# xref:
# 	./rebar clean compile xref skip_deps=true

# test:
# 	rm -rf .eunit
# 	./rebar eunit skip_deps=true

# release:
#
# 	make clean
# 	make
# 	./rebar3 generate overlay_vars=`pwd`/rel/${NODE}.config
#
# upgrade_rel:
# 	make clean
# 	make
# 	./rebar generate overlay_vars=${NODE}.config
# 	./rebar generate-appup previous_release=${PRE}
# 	./rebar generate-upgrade previous_release=${PRE}
# 	cd rel && mv worker_*.tar.gz ${PRE}/releases/; cd ..

## add your dependecies here. --apps [depencencies from otp] -r [our deps]
## fixme statebox and yaws doesn't work with dialyzer right now.
# init_dialyzer:
# 	rm -rf deps/statebox/ebin/*beam
# 	dialyzer --apps ssl xmerl stdlib kernel inets crypto public_key -r deps --build_plt --output_plt dialyzer.plt
#
# dialyzer:
# 	rm -rf apps/*/.eunit
# 	dialyzer -r apps --plt dialyzer.plt
#
# help:
# 	@echo "Commands:"
# 	@echo "  Build:  \t make"
# 	@echo "  Test:   \t make test"
# 	@echo "  Release:\t make release NODE=<name>"
# 	@echo "          \t - name should match a file in rel/<name>.config"
# 	@echo "  Upgrade:\t make upgrade_rel NODE=<name> PRE=<previous_rel>"
# 	@echo "          \t - name should match a file in rel/<name>.config"
# 	@echo "          \t - previous_rel should match a directory under rel/"

.PHONY: build run run-dev run-prod

build:
	rebar3 compile

run: run-dev

run-dev:
	set -a; \
	source devops/env/raptor.env; \
	source devops/env/.env; \
	set +a; \
	rebar3 shell --config config/sys.config

run-prod:
	set -a; \
	source devops/env/raptor.env; \
	source devops/env/.env; \
	set +a; \
	rebar3 shell --config config/sys.prod.config
