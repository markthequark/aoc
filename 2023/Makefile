shell:
	@iex -S mix

run: build
	@./advent_of_code $(filter-out $@, $(MAKECMDGOALS))

format:
	@mix format

build:
	@mix deps.get
	@mix deps.compile
	@MIX_ENV=prod mix escript.build

test:
	@mix test --trace

new_day:
	@mix new_day

ci:
	@mix format --check-formatted
	@mix compile --warnings-as-errors
	@mix test --trace --warnings-as-errors

%:
	@:

.PHONY: shell run format build test new_day ci
