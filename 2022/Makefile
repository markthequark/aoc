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

%:
	@:

.PHONY: shell run format build test
