REPO            ?= rcouch
PKG_REVISION    ?= $(shell git describe --tags)
PKG_BUILD        = 1
OVERLAY_VARS ?=
PACKAGE_NAME=rcouch
RELDIR=$(BASE_DIR)/rel/$(PACKAGE_NAME)
BASE_DIR = $(shell pwd)
REBAR := $(shell which rebar)
ifeq ($(REBAR),)
REBAR = $(BASE_DIR)/rebar
endif

.PHONY: rel deps rebar

include tools.mk


rel: compile
	@echo "==> generate rcouch release"
	@$(REBAR) generate $(OVERLAY_VARS)

relclean: reldocclean
	@rm -rf rel/rcouch

#
# DOCS
#


DOC_RELDIR=$(RELDIR)/share/doc
reldoc: reldocclean doc
	mkdir -p $(DOC_RELDIR)
	cp -r $(DOC_BUILDDIR)/html $(DOC_RELDIR)
	cp -r $(DOC_BUILDDIR)/latex/CouchDB.pdf $(DOC_RELDIR)
	cp -r $(DOC_BUILDDIR)/texinfo/CouchDB.info $(DOC_RELDIR)

reldocclean:
	rm -rf $(DOC_RELDIR)
