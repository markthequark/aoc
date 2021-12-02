all: format build run

format:
	@rebar3 format

build:
	@rebar3 escriptize

run: _build/default/bin/sonar_sweep
	@_build/default/bin/sonar_sweep

shell:
	@rebar3 shell
