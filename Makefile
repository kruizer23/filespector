# Set the application name.
APPNAME := filespector

# Set the directories.
SRCDIR := sources
LOCDIR := $(SRCDIR)/languages
DOCDIR := docs
MANDIR := $(DOCDIR)/man
ASSSETSDIR := assets

# Appstream related configuration.
APPDIR := $(ASSSETSDIR)/appstream
APPID  := io.github.kruizer23.$(APPNAME)

# Set the default prefix. Can be overridden like this "make PREFIX=/usr/local".
ifeq ($(PREFIX),)
  PREFIX := /usr
endif	

# Set the default build mode. Can be overridden like this "make BUILDMODE=Debug".
ifeq ($(BUILDMODE),)
  BUILDMODE := Release
endif	

# Set the default widget set. Can be overridden like this "make WIDGETSET=qt5".
ifeq ($(WIDGETSET),)
  WIDGETSET := gtk2
endif	

# Configure dependent executables with their full directory.
LAZBUILD := $(shell which lazbuild)
MSGFMT := $(shell which msgfmt)
PANDOC := $(shell which pandoc)

# Collect all translation files.
POFILES := $(wildcard $(LOCDIR)/*.po)
# Build list with machine object translation files.
MOFILES := $(patsubst %.po,%.mo,$(POFILES))
# Build list with installation files of machine object translation files.
MOINSTFILES := $(patsubst $(LOCDIR)/$(APPNAME).%.mo,$(DESTDIR)$(PREFIX)/share/locale/%/LC_MESSAGES/$(APPNAME).mo,$(MOFILES))

# The "all" target for building the application and the machine object translation files.	
.PHONY: all	
all: $(SRCDIR)/$(APPNAME) $(MOFILES) $(MANDIR)/$(APPNAME).1

# Target for building the application.
$(SRCDIR)/$(APPNAME): $(SRCDIR)/$(APPNAME).lpi 
	$(LAZBUILD) --build-mode="$(BUILDMODE)" --widgetset=$(WIDGETSET) $(SRCDIR)/$(APPNAME).lpi
	
# Target for creating the MAN page from the markdown formatted source document.
$(MANDIR)/$(APPNAME).1: $(MANDIR)/$(APPNAME).1.md
	$(PANDOC) $< -s -t man -o $@
	
# Pattern rule for building the machine object translation files.	
$(MOFILES): %.mo: %.po
	$(MSGFMT) -o $@ $<

# Target for cleaning up all the files created by the "all" target.
.PHONY: clean
clean:
	rm -f $(SRCDIR)/$(APPNAME)
	rm -f $(SRCDIR)/$(APPNAME).res
	rm -f $(MANDIR)/$(APPNAME).1
	rm -rf $(SRCDIR)/lib
	rm -rf $(LOCDIR)/*.mo	

# Pattern rule for installing the machine object translation files.
$(MOINSTFILES): $(DESTDIR)$(PREFIX)/share/locale/%/LC_MESSAGES/$(APPNAME).mo: $(LOCDIR)/$(APPNAME).%.mo
	install -d -m 755 $(dir $@)
	install -pm 644 $< $@

# Target to install the application. 
.PHONY: install
install: $(MOINSTFILES)
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(SRCDIR)/$(APPNAME) $(DESTDIR)$(PREFIX)/bin/
	install -d $(DESTDIR)$(PREFIX)/share/pixmaps
	install -m 644 $(ASSSETSDIR)/images/$(APPNAME).png $(DESTDIR)$(PREFIX)/share/pixmaps
	install -d $(DESTDIR)$(PREFIX)/share/applications
	install -m 644 $(APPDIR)/$(APPID).desktop $(DESTDIR)$(PREFIX)/share/applications
	install -d $(DESTDIR)$(PREFIX)/share/metainfo
	install -m 644 $(APPDIR)/$(APPID).metainfo.xml $(DESTDIR)$(PREFIX)/share/metainfo

# Target to uninstall the application. 
.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/$(APPNAME)
	rm -f $(DESTDIR)$(PREFIX)/share/pixmaps/$(APPNAME).png
	rm -f $(DESTDIR)$(PREFIX)/share/applications/$(APPID).desktop
	rm -f $(DESTDIR)$(PREFIX)/share/metainfo/$(APPID).metainfo.xml	
	if [ -d "$(DESTDIR)$(PREFIX)/share/$(APPNAME)" ]; then rmdir $(DESTDIR)$(PREFIX)/share/$(APPNAME); fi	
	rm -f $(MOINSTFILES)

