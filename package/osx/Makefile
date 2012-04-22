ARCH        = $(shell uname -m)
PKGNAME     = $(APP)-$(REVISION)-osx-$(ARCH).tar.gz
DMGNAME		= rcouchx-$(REVISION).dmg
ZIPNAME		= rcouchx-$(REVISION).zip
DMGFOLDER	= packages/$(APP)-$(REVISION)

# simply tar up the rel directory and sha the file
build: clean buildrel
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILDDIR)/$(APP)-$(REVISION) && \
		cp -R rel/rcouch $(APP)-$(REVISION) && \
		tar -czf ../../packages/$(PKGNAME) $(APP)-$(REVISION)
	cd packages && \
		for tarfile in `ls *.gz`; do \
			shasum -a 256 $${tarfile} > $${tarfile}.sha \
		; reldone

dmg: cleanrcouchx buildrcouchx
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cp -f osx/README.txt $(BUILDDIR)/$(APP)-$(REVISION)/
	cp -f ../LICENSE $(BUILDDIR)/$(APP)-$(REVISION)/LICENSE.txt
	cp -f osx/dmg-background.png $(BUILDDIR)/$(APP)-$(REVISION)
	cp -f osx/setviewoptions.applescript $(BUILDDIR)/$(APP)-$(REVISION)
	cp -f osx/makedmg.sh $(BUILDDIR)/$(APP)-$(REVISION)/
	cp -f "../contrib/rcouchx/rcouch.icns" $(BUILDDIR)/$(APP)-$(REVISION)/
	(cd $(BUILDDIR)/$(APP)-$(REVISION) &&  \
		./makedmg.sh "$(DMGNAME)" "rcouchx.app" "rcouchx" dmg-background.png rcouch.icns)
	cp -f $(BUILDDIR)/$(APP)-$(REVISION)/$(DMGNAME) packages/
	cd packages && \
		zip $(ZIPNAME) $(DMGNAME)
	cd packages && \
		shasum -a 256 $(ZIPNAME) > $(ZIPNAME).sha

clean:
	@rm -rf $(BUILDDIR)

cleanrcouchx: clean
	@rm -rf packages

# Build the release we need to package
buildrel: $(BUILDDIR)/$(APP)-$(REVISION)
	cd $^ && $(MAKE) deps compile rel


buildrcouchx: $(BUILDDIR)/$(APP)-$(REVISION)
	cd $^ && $(MAKE) deps rcouchx

$(BUILDDIR)/$(APP)-$(REVISION): $(BUILDDIR) $(APP)-$(REVISION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(REVISION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	@echo
