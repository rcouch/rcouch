ARCH= $(shell uname -m)
REPO=rcouch
RCOUCH_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(RCOUCH_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)
WITHOUT_CURL?=1
REBAR?=./rebar

DESTDIR?=
DISTDIR=       rel/archive

.PHONY: rel deps

all: deps compile

compile:
	@WITHOUT_CURL=$(WITHOUT_CURL) $(REBAR) compile

deps:
	@$(REBAR) get-deps

clean: rcouchxclean
	@$(REBAR) clean

distclean: clean relclean
	@$(REBAR) delete-deps

rel: relclean deps
	@WITHOUT_CURL=$(WITHOUT_CURL) $(REBAR) compile generate

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


rcouchx: rel rcouchxbuild

rcouchxbuild: rcouchxclean
	@cp -R contrib/rcouchx rcouchx-build
	@xcodebuild -project contrib/rcouchx/rcouchx.xcodeproj
	@cp -R contrib/rcouchx/build/Release/rcouchx.app .

rcouchxclean:
	@xcodebuild -project contrib/rcouchx/rcouchx.xcodeproj clean
	@rm -rf contrib/rcouchx/build
	@rm -rf rcouchx.app

rcouchxdmg: dist
	$(MAKE) -C package rcouchx


.PHONY: package

export PKG_VERSION REPO REVISION RCOUCH_TAG

