DESTDIR?=
DISTDIR=       rel/archive

.PHONY: rel stagedevrel deps

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean: devclean
	@./rebar clean

distclean: clean devclean relclean
	@./rebar delete-deps

rel: deps
	@./rebar compile generate

relclean:
	@rm -rf rel/refuge
	
##
## dev targets
##

dev: devclean all devrel
	@echo "\n\
Development nodes are built, and can be started using ./dev/dev[123]/bin/refuge.\n"

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	@mkdir -p dev
	@(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devclean: 
	@rm -rf dev
