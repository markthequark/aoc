shell:
	@iex -S mix

run: build
	@./advent_of_code $(filter-out $@, $(MAKECMDGOALS))

format:
	@mix format

build:
	@mix escript.build

%:
	@:

.PHONY: shell run format build
