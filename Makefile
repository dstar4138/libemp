#
# Extensible Monitoring Platform Makefile
#
ERL=$(shell which erl)
ERLFLAGS= -pa ./ebin #-pa ./deps/*/ebin #-pa ./plugins/ebin
EMPFLAG= "-eval 'libemp:start().'"
REBAR=$(shell which rebar || echo $(CURDIR)/bin/rebar)
RELTOOL_CFG=$(CURDIR)/reltool.config
RELDIR=$(CURDIR)/rel
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
DIALYZER_WARN=-Wrace_conditions -Werror_handling -Wunmatched_returns -Wunderspecs 
DIALYZER_ARGS=--plt $(DEPSOLVER_PLT) --no_check_plt $(DIALYZER_WARN)
TYPER_ARGS=--plt $(DEPSOLVER_PLT) 

.PHONY: libemp release deps docs testall eunit diailyzer typer clean distclean

##BUILDING
libemp: deps startemp.sh loaderl.sh
	$(REBAR) compile

startemp.sh:
	touch startemp.sh
	echo "#!/bin/sh" > startemp.sh
	echo $(ERL) $(ERLFLAGS) $(EMPFLAG) >> startemp.sh
	chmod +x startemp.sh

loaderl.sh:
	touch loaderl.sh
	echo "#!/bin/sh" > loaderl.sh
	echo $(ERL) $(ERLFLAGS) >> loaderl.sh
	chmod +x loaderl.sh

release: libemp
	mkdir $(RELDIR)
	cd $(RELDIR)
	$(REBAR) create-node nodeid=libemp
	cp $(RELTOOL_CFG) $(RELDIR)
	$(REBAR) generate

deps:
	$(REBAR) get-deps

docs: 
	$(REBAR) doc

##TESTING
testall: eunit dialyzer typer

eunit:
	-$(REBAR) eunit

$(DEPSOLVER_PLT):
	-dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	         --apps erts kernel stdlib crypto public_key sasl mnesia

dialyzer: $(DEPSOLVER_PLT) deps
	# Since some of our dependencies won't have make scripts, so make a best effort.
	-$(foreach d, $(shell ls -d deps/*), \
		dialyzer $(DIALYZER_ARGS) --src $(d)/src \
				-I $(d)/include -I $(CURDIR)/deps/empd/include ;)
	-dialyzer $(DIALYZER_ARGS) --src src

typer: $(DEPSOLVER_PLT) deps
	# Since some of our dependencies won't have make scripts, so make a best effort.
	-$(foreach d, $(shell ls -d deps/*), \
		typer $(TYPER_ARGS) -r $(d)/src \
			-I $(d)/include -I $(CURDIR)/deps/empd/include ;)
	-typer $(TYPER_ARGS) -r ./src

##CLEANING
clean:
	-$(REBAR) clean
	-rm startemp.sh
	-rm loaderl.sh

distclean: clean
	$(REBAR) delete-deps
	-rm $(DEPSOLVER_PLT)

