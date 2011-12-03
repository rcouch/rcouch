# requires GNU make
PKG		 = REFUGEIO$(APP)
# possible ARCH values are i386, sparc, all
ARCH		 = $(shell uname -p)
SOLARIS_VER	?= $(shell echo "$(KERNEL)" | sed -e 's/^5\.//')
PKGFILE		 = $(PKG)-$(REVISION)-$(RELEASE)-$(DISTRO)$(SOLARIS_VER)-$(ARCH).pkg

build: buildrel depend pkginfo prototype
	cp $(PKGERDIR)/copyright $(PKGERDIR)/preinstall $(PKGERDIR)/*.preserve .
	mkdir -p pkgbuild packages
	pkgmk -o -d pkgbuild -a $(ARCH)
	touch packages/$(PKGFILE)
	pkgtrans -s pkgbuild packages/$(PKGFILE) $(PKG) 
	rm -r pkgbuild/$(PKG)
	gzip packages/$(PKGFILE)
	chmod 0644 $(CURDIR)/packages/$(PKGFILE).gz
	@echo
	@echo Wrote: $(CURDIR)/packages/$(PKGFILE).gz
	@echo
	cd packages && echo "`digest -a sha256 $(PKGFILE).gz`  $(PKGFILE).gz" > $(PKGFILE).gz.sha

# Build the release we need to package
buildrel:
	@# Make sure we set our EUID properly
	echo "" >> $(RCOUCH_PATH)/rel/vars.config
	echo '{runner_user, "rcouch"}.' >> $(RCOUCH_PATH)/rel/rcouch.config
	@# Ye Olde Bourne Shell on Solaris means we have to do it old school
	echo "Using `which erl` to build"; \
	$(MAKE) -C $(RCOUCH_PATH) deps rel
	chmod 0755 $(RCOUCH_PATH)/rel/rcouch/bin/* \
		$(RCOUCH_PATH)/rel/rcouch/erts-*/bin/*

depend:
	cp $(PKGERDIR)/depend .
	if [ $(DISTRO) = "OpenSolaris" ]; then \
		echo "P SUNWgawk   GNU implementation of awk" >> depend; \
	fi

pkginfo:
	sed -e 's/@@VERSION@@/$(REVISION)-$(RELEASE)/g' \
		-e 's/@@PKG@@/$(PKG)/g' \
		-e 's/@@PKGNAME@@/$(APP)/g' \
		< $(PKGERDIR)/pkginfo.tmpl > pkginfo

# NOTE!  The instances of rcouch below shouldn't change
prototype:
	echo "i pkginfo" > prototype
	echo "i copyright" >> prototype
	echo "i depend" >> prototype
	echo "i preinstall" >> prototype
	echo "i i.preserve" >> prototype
	echo "i r.preserve" >> prototype
	echo '' >> prototype
	pkgproto $(RCOUCH_PATH)/rel/rcouch=rcouch >> prototype
	sed -e "s/ $(LOGNAME) .*$$/ rcouch rcouch/" \
	    -e 's/f none rcouch\/etc/e preserve rcouch\/etc/' prototype > prototype.tmp && mv prototype.tmp prototype

$(PKGERDIR)/pkgclean:
	rm -rf copyright depend pkgbuild pkginfo prototype

