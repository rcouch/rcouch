BASE_DIR = $(shell pwd)
SUPPORT_DIR=$(BASE_DIR)/support
ERLC ?= $(shell which erlc)
ESCRIPT ?= $(shell which escript)


REBAR_BIN := $(shell which rebar)
ifeq ($(REBAR_BIN),)
REBAR_BIN = $(BASE_DIR)/rebar
endif

$(if $(ERLC),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

$(if $(ESCRIPT),,$(warning "Warning: No escript found in your path, this will probably not work"))

COUCHDB_STATIC=1
ifeq ($(libs), shared)
	COUCHDB_STATIC=0
endif
export COUCHDB_STATIC

USE_STATIC_ICU=0
ifeq ($(icu), static)
	USE_STATIC_ICU=1
endif
export USE_STATIC_ICU


.PHONY: rel deps rebar

all: deps compile

compile:
	@$(REBAR_BIN) compile

deps: rebarbuild
	@$(REBAR_BIN) get-deps

clean: docclean
	@$(REBAR_BIN) clean

check: test testjs

#
# rebar
#

rebarbuild:
	@(test ! -e $(BASE_DIR)/support/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(BASE_DIR)/support/rebar && \
		$(ESCRIPT) bootstrap || true)
	@cp $(BASE_DIR)/support/rebar/rebar $(BASE_DIR)/rebar

rebarclean:
	@(cd $(BASE_DIR)/support/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)

#
# DOCS
#

DOC_SRCDIR=$(BASE_DIR)/share/doc/src
DOC_BUILDDIR=$(BASE_DIR)/share/doc/build
SPHINXOPTS = -n -c $(DOC_SRCDIR) \
			 -A local=1 \
			 $(DOC_SRCDIR)

doc: html pdf texinfo

html:
	@mkdir -p $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b html $(SPHINXOPTS) $(DOC_BUILDDIR)/html

pdf:
	@mkdir -p $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b latex $(SPHINXOPTS) $(DOC_BUILDDIR)/latex
	$(MAKE) -C $(DOC_BUILDDIR)/latex all-pdf

texinfo:
	@mkdir -p $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b texinfo $(SPHINXOPTS) $(DOC_BUILDDIR)/texinfo
	$(MAKE) -C $(DOC_BUILDDIR)/texinfo info

docclean:
	rm -rf $(DOC_BUILDDIR)/textinfo
	rm -rf $(DOC_BUILDDIR)/latex
	rm -rf $(DOC_BUILDDIR)/html
	rm -rf $(DOC_BUILDDIR)/doctrees

#
# TESTS
#
COUCHDB_ETAP_DIR=$(BASE_DIR)/test/etap
export COUCHDB_ETAP_DIR


ERL_FLAGS=-pa $(BASE_DIR)/src/*/ebin -pa $(COUCHDB_ETAP_DIR)
export ERL_FLAGS

test: testbuild
	@echo "==> test couch_collate"
	@cd $(BASE_DIR)/src/couch_collate && \
		prove $(BASE_DIR)/src/couch_collate/t/*.t
	@echo "==> test couch core"
	@prove $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove $(BASE_DIR)/src/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove $(BASE_DIR)/src/couch_replicator/test/*.t

verbose-test: testbuild
	@echo "==> test couch_collate"
	@cd $(BASE_DIR)/src/couch_collate && \
		prove -v $(BASE_DIR)/src/couch_collate/t/*.t
	@echo "==> test couch core"
	@prove -v $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove -v $(BASE_DIR)/src/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove -v $(BASE_DIR)/src/couch_replicator/test/*.t

testjs: testbuild
	@$(ESCRIPT) $(BASE_DIR)/test/javascript/test_js.escript

testbuild: testclean
	@echo "==> init test environement"
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/etap.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_web.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_util.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/mustache.erl
	@cc -DBSD_SOURCE $(COUCHDB_ETAP_DIR)/test_cfg_register.c \
		-o $(COUCHDB_ETAP_DIR)/test_cfg_register
	@mkdir -p $(BASE_DIR)/test/out/data
	@mkdir -p $(BASE_DIR)/test/out/bin
	@mkdir -p $(BASE_DIR)/test/out/share
	@mkdir -p $(BASE_DIR)/test/out/log
	@cp $(BASE_DIR)/src/couch/priv/couchjs $(BASE_DIR)/test/out/bin/
	@cp -r $(BASE_DIR)/share/server $(BASE_DIR)/test/out/share
	@cp -r $(BASE_DIR)/share/www $(BASE_DIR)/test/out/share
	@cp $(BASE_DIR)/etc/rcouch/local.ini $(BASE_DIR)/test/out/

testclean:
	@rm -rf $(COUCHDB_ETAP_DIR)/*.beam
	@rm -rf $(BASE_DIR)/test/out
	@rm -rf $(COUCHDB_ETAP_DIR)/test_cfg_register
	@rm -rf $(COUCHDB_ETAP_DIR)/*.o

.PHONY: rebar
