.PHONY: test deps

REBAR_CONFIG ?= rebar.config


all: deps compile test

# dialyzer --build_plt --apps erts kernel stdlib mnesia crypto public_key snmp reltool
# dialyzer --add_to_plt --plt ~/.dialyzer_plt --output_plt radius.plt -c .
# dialyzer -c ebin -I include -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

dialyze: compile
	dialyzer --add_to_plt --plt ~/.dialyzer_plt --output_plt radius.plt -c .
	dialyzer -c ebin -I include

compile:
	@./rebar -C ${REBAR_CONFIG} compile skip_deps=true
	@./rebar -C ${REBAR_CONFIG} xref skip_deps=true

deps:
	@./rebar -C ${REBAR_CONFIG} update-deps
	@./rebar -C ${REBAR_CONFIG} get-deps
	@./rebar -C ${REBAR_CONFIG} compile

clean:
	@./rebar -C ${REBAR_CONFIG} clean skip_deps=true

test:
	@./rebar -C ${REBAR_CONFIG} eunit skip_deps=true

distclean: clean
	@./rebar -C ${REBAR_CONFIG} delete-deps
