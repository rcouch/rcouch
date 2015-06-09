REBAR ?= ./rebar3


all: compile

compile:
	@$(REBAR) compile	

rel: compile
	@$(REBAR) release 

clean: 
	@$(REBAR) clean

dist: 
	@$(REBAR) tar
