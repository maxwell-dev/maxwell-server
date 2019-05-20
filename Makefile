.PHONY : default compile release auto test clean

REBAR=rebar3

default: auto

compile:
	${REBAR} get-deps
	${REBAR} compile

release: compile
	${REBAR} release

auto: release
	ERL_FLAGS="-args_file config/vm.args" ${REBAR} auto

test:
	ERL_FLAGS="-args_file config/vm.args" ${REBAR} eunit

clean:
	${REBAR} clean
	rm -f rebar.lock
