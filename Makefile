all: compile

compile: deps
	@rebar compile

deps:
	@rebar get-deps

tests:
	@rebar eunit

clean:
	@rebar clean

.PHONY: clean compile tests
