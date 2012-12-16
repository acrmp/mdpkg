REBAR := ./rebar

.PHONY: test clean

all: compile

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

test:
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean

run:
	ERL_LIBS=deps erl -pa ebin -noshell -start_clean -s metapackage start
