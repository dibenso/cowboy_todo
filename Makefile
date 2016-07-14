all: compile

compile:
	./rebar3 compile

release:
	./rebar3 release

run:
	./rebar3 shell
	