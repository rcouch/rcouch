REPO            ?= rcouch
PKG_REVISION    ?= $(shell git describe --tags)
PKG_BUILD        = 1
BASE_DIR = $(shell pwd)
SUPPORT_DIR=$(BASE_DIR)/support
ERLC ?= $(shell which erlc)
ESCRIPT ?= $(shell which escript)
OVERLAY_VARS    ?=

REBAR := $(shell which rebar)

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

all: compile

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean: docclean
	@$(REBAR) clean

check: test testjs

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


ERL_FLAGS=-pa $(BASE_DIR)/deps/*/ebin -pa $(COUCHDB_ETAP_DIR)
export ERL_FLAGS

test: testbuild
	@echo "==> test couch core"
	@prove $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove $(BASE_DIR)/deps/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove $(BASE_DIR)/deps/couch_replicator/test/*.t

verbose-test: testbuild
	@echo "==> test couch_collate"
	@cd $(BASE_DIR)/deps/couch_collate && \
		prove -v $(BASE_DIR)/deps/couch_collate/t/*.t
	@echo "==> test couch core"
	@prove -v $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove -v $(BASE_DIR)/deps/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove -v $(BASE_DIR)/deps/couch_replicator/test/*.t

core-tests:
	@echo "==> test couch core"
	@prove -v $(COUCHDB_ETAP_DIR)/*.t

view-tests: testbuild
	@echo "==> test couch_mrview"
	@prove -v $(BASE_DIR)/deps/couch_mrview/test/*.t

replicator-test: testbuild
	@echo "==> test couch_replicator"
	@prove -v $(BASE_DIR)/deps/couch_replicator/test/*.t



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
	@cp $(BASE_DIR)/deps/couch/priv/couchjs $(BASE_DIR)/test/out/bin/
	@cp -r $(BASE_DIR)/share/server $(BASE_DIR)/test/out/share
	@cp -r $(BASE_DIR)/share/www $(BASE_DIR)/test/out/share
	@cp $(BASE_DIR)/etc/rcouch/local.ini $(BASE_DIR)/test/out/

testclean:
	@rm -rf $(COUCHDB_ETAP_DIR)/*.beam
	@rm -rf $(BASE_DIR)/test/out
	@rm -rf $(COUCHDB_ETAP_DIR)/test_cfg_register
	@rm -rf $(COUCHDB_ETAP_DIR)/*.o

##
## Version and naming variables for distribution and packaging
##

# Tag from git with style <tagname>-<commits_since_tag>-<current_commit_hash>
# Ex: When on a tag:            rcouch-1.0.3   (no commits since tag)
#     For most normal Commits:  rcouch-1.1.0pre1-27-g1170096
#                                 Last tag:          rcouch-1.1.0pre1
#                                 Commits since tag: 27
#                                 Hash of commit:    g1170096
REPO_TAG 	:= $(shell git describe --tags)

# Split off repo name
# Changes to 1.0.3 or 1.1.0pre1-27-g1170096 from example above
REVISION = $(shell echo $(REPO_TAG) | sed -e 's/^$(REPO)-//')

# Primary version identifier, strip off commmit information
# Changes to 1.0.3 or 1.1.0pre1 from example above
MAJOR_VERSION	?= $(shell echo $(REVISION) | sed -e 's/\([0-9.]*\)-.*/\1/')


##
## Release tarball creation
## Generates a tarball that includes all the deps sources so no checkouts are necessary
##

# Use git archive make a clean copy of a repository at a current
# revision and copy to a new directory
archive_git = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

# Alternative to git archive to remove .git directory, but not any
# other files outside of the source tree (used for eleveldb which
# brings in leveldb)
clean_git = cp -R ../../$(1) $(2)/deps/ && find $(2)/$(1) -name .git -type d | xargs rm -rf

# Determines which function to call.  eleveldb is treated as a special case
archive = $(call archive_git,$(1),$(2))


# Checkout tag, fetch deps (so we don't have to do it multiple times) and collect
# the version of all the dependencies into the MANIFEST_FILE
CLONEDIR ?= rcouch-clone
MANIFEST_FILE ?= dependency_manifest.git
get_dist_deps = mkdir distdir && \
                git clone . distdir/$(CLONEDIR) && \
                cd distdir/$(CLONEDIR) && \
                git checkout $(REPO_TAG) && \
                $(MAKE) deps && \
                echo "- Dependencies and their tags at build time of $(REPO) at $(REPO_TAG)" > $(MANIFEST_FILE) && \
                for dep in deps/*; do \
                    cd $${dep} && \
                    printf "$${dep} version `git describe --long --tags 2>/dev/null || git rev-parse HEAD`\n" >> ../../$(MANIFEST_FILE) && \
                    cd ../..; done && \
                LC_ALL=POSIX && export LC_ALL && sort $(MANIFEST_FILE) > $(MANIFEST_FILE).tmp && mv $(MANIFEST_FILE).tmp $(MANIFEST_FILE);


# Name resulting directory & tar file based on current status of the git tag
# If it is a tagged release (PKG_VERSION == MAJOR_VERSION), use the toplevel
#   tag as the package name, otherwise generate a unique hash of all the
#   dependencies revisions to make the package name unique.
#   This enables the toplevel repository package to change names
#   when underlying dependencies change.
NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
ifeq ($(REVISION), $(MAJOR_VERSION))
PKG_ID := $(REPO_TAG)
else
PKG_ID = $(REPO)-$(MAJOR_VERSION)-$(NAME_HASH)
endif

# To ensure a clean build, copy the CLONEDIR at a specific tag to a new directory
#  which will be the basis of the src tar file (and packages)
# The vsn.git file is required by rebar to be able to build from the resulting
#  tar file
build_clean_dir = cd distdir/$(CLONEDIR) && \
                  $(call archive_git,$(PKG_ID),..) && \
                  cp $(MANIFEST_FILE) ../$(PKG_ID)/ && \
                  mkdir ../$(PKG_ID)/deps && \
                  for dep in deps/*; do \
                      cd $${dep} && \
                           $(call archive,$${dep},../../../$(PKG_ID)) && \
                           mkdir -p ../../../$(PKG_ID)/$${dep}/priv && \
                           printf "`git describe --long --tags 2>/dev/null || git rev-parse HEAD`" > ../../../$(PKG_ID)/$${dep}/priv/vsn.git && \
                           cd ../..; \
                  done


distdir/$(CLONEDIR)/$(MANIFEST_FILE):
	$(if $(REPO_TAG), $(call get_dist_deps), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

distdir/$(PKG_ID): distdir/$(CLONEDIR)/$(MANIFEST_FILE)
	$(call build_clean_dir)

distdir/$(PKG_ID).tar.gz: distdir/$(PKG_ID)
	tar -C distdir -czf distdir/$(PKG_ID).tar.gz $(PKG_ID)

dist: distdir/$(PKG_ID).tar.gz
	cp distdir/$(PKG_ID).tar.gz .

ballclean:
	rm -rf $(PKG_ID).tar.gz distdir

pkgclean: ballclean
	rm -rf package

##
## Packaging targets
##

# Yes another variable, this one is repo-<generatedhash
# which differs from $REVISION that is repo-<commitcount>-<commitsha>
PKG_VERSION = $(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

package: distdir/$(PKG_ID).tar.gz
	ln -s distdir package
	$(MAKE) -C package -f $(PKG_ID)/deps/node_package/Makefile

.PHONY: package rebar
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE
