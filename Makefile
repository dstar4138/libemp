#
# Extensible Monitoring Platform Makefile
#
ERL=$(shell which erl)
REBAR=$(shell which rebar || echo $(CURDIR)/bin/rebar)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
DIALYZER_WARN=-Wrace_conditions -Werror_handling -Wunmatched_returns -Wunderspecs 
DIALYZER_ARGS=--plt $(DEPSOLVER_PLT) --no_check_plt $(DIALYZER_WARN)
TYPER_ARGS=--plt $(DEPSOLVER_PLT) 
USED_APPS=kernel stdlib erts crypto hipe sasl public_key

.PHONY: libemp release deps docs testall eunit dialyzer typer clean distclean

##BUILDING
libemp: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

docs: 
	$(REBAR) doc

##TESTING
testall: libemp eunit dialyzer typer

eunit:
	$(REBAR) eunit

$(DEPSOLVER_PLT):
	-dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt --apps $(USED_APPS)

dialyzer: $(DEPSOLVER_PLT) deps
	-dialyzer $(DIALYZER_ARGS) --src ./src -I ./include

typer: $(DEPSOLVER_PLT) deps
	-typer $(TYPER_ARGS) -r ./src -I ./include

##CLEANING
clean:
	-$(REBAR) clean

distclean: clean
	-$(REBAR) delete-deps
	-rm $(DEPSOLVER_PLT)
	-rm -rf ebin
	-rm -rf doc

