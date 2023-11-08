all: format build
	@_build/default/bin/advent_of_code 15

format:
	@rebar3 format

build:
	@rebar3 escriptize

run: _build/default/bin/advent_of_code
	@_build/default/bin/advent_of_code $(filter-out $@, $(MAKECMDGOALS))

shell:
	@rebar3 shell

%:
	@:
