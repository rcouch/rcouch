REPO=rcouch
RCOUCH_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(RCOUCH_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)
WITHOUT_CURL?=1

DESTDIR?=
DISTDIR=       rel/archive

.PHONY: rel deps

all: deps compile

compile:
	@WITHOUT_CURL=$(WITHOUT_CURL) rebar compile

deps:
	@rebar get-deps

clean:
	@rebar clean

distclean: clean relclean
	@rebar delete-deps

rel: relclean deps
	@WITHOUT_CURL=$(WITHOUT_CURL) rebar compile generate

relclean:
	@rm -rf rel/rcouch

##
## release tarbals
##

archive = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

buildtar = mkdir distdir && \
		 git clone . distdir/rcouch-clone && \
		 cd distdir/rcouch-clone && \
		 git checkout $(RCOUCH_TAG) && \
		 $(call archive,$(RCOUCH_TAG),..) && \
		 mkdir ../$(RCOUCH_TAG)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(RCOUCH_TAG)) && \
                     mkdir -p ../../../$(RCOUCH_TAG)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(RCOUCH_TAG)/$${dep}/priv/git.vsn && \
                     cd ../..; done

distdir:
	$(if $(RCOUCH_TAG), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

dist $(RCOUCH_TAG).tar.gz: distdir
	cd distdir; \
	tar czf ../$(RCOUCH_TAG).tar.gz $(RCOUCH_TAG)


package: dist
	$(MAKE) -C package package

pkgclean:
	$(MAKE) -C package pkgclean

.PHONY: package

export PKG_VERSION REPO REVISION RCOUCH_TAG

