#
# Extensible Monitoring Platform Makefile
#
ERL=$(shell which erl)
REBAR=$(shell which rebar3 || echo $(CURDIR)/bin/rebar3)

.PHONY: libemp release deps docs testall eunit dialyzer typer clean distclean

##BUILDING
libemp:
	$(REBAR) compile

docs:
	$(REBAR) edoc

##TESTING
shell:
	$(REBAR) as test shell

prodshell:
	$(REBAR) as prod shell

test: eunit

eunit:
	$(REBAR) eunit --cover

dialyzer:
	$(REBAR) dialyzer

##CLEANING
clean:
	$(REBAR) clean

distclean: clean
	-$(REBAR) clean --all
	-rm $(DEPSOLVER_PLT)
	-rm -rf _build
	-rm -rf _checkouts

## RELEASE
release:
	$(REBAR) as prod,native release

tar:
	$(REBAR) as prod tar
