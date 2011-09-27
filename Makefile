REPO=refuge
REFUGE_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(REFUGE_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)

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

rel: relclean deps
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

##
## release tarbals
##

archive = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

buildtar = mkdir distdir && \
		 git clone . distdir/refuge-clone && \
		 cd distdir/refuge-clone && \
		 git checkout $(REFUGE_TAG) && \
		 $(call archive,$(REFUGE_TAG),..) && \
		 mkdir ../$(REFUGE_TAG)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(REFUGE_TAG)) && \
                     mkdir -p ../../../$(REFUGE_TAG)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(REFUGE_TAG)/$${dep}/priv/git.vsn && \
                     cd ../..; done

distdir:
	$(if $(REFUGE_TAG), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

dist $(REFUGE_TAG).tar.gz: distdir
	cd distdir; \
	tar czf ../$(REFUGE_TAG).tar.gz $(REFUGE_TAG)


package: dist
	$(MAKE) -C package package

pkgclean:
	$(MAKE) -C package pkgclean

.PHONY: package

export PKG_VERSION REPO REVISION REFUGE_TAG

