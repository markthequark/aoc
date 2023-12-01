shell:
	@iex -S mix

run: build
	@./advent_of_code $(filter-out $@, $(MAKECMDGOALS))

format:
	@mix format

build:
	@mix deps.get
	@mix deps.compile
	@mix escript.build

test:
	@mix test --trace

new_day:
	@mix new_day

%:
	@:

.PHONY: shell run format build test new_day
