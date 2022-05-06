#****************************************************************************************
#|  Description: Makefile for a FileSpector application.
#|    File Name: Makefile
#|
#|---------------------------------------------------------------------------------------
#|                          C O P Y R I G H T
#|---------------------------------------------------------------------------------------
#|           Copyright (c) 2022 by Frank Voorburg   All rights reserved
#|
#|   This software has been carefully tested, but is not guaranteed for any particular
#| purpose. The author does not offer any warranties and does not guarantee the accuracy,
#|   adequacy, or completeness of the software and is not responsible for any errors or
#|              omissions or the results obtained from use of the software.
#|
#|---------------------------------------------------------------------------------------
#|                            L I C E N S E
#|---------------------------------------------------------------------------------------
#| This file is part of FileSpector. FileSpector is free software: you can redistribute
#| it and/or modify it under the terms of the GNU General Public License as published by
#| the Free Software Foundation, either version 3 of the License, or (at your option) any
#| later version.
#|
#| FileSpector is distributed in the hope that it will be useful, but WITHOUT ANY
#| WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#| PARTICULAR PURPOSE. See the GNU General Public License for more details.
#|
#| You should have received a copy of the GNU General Public License along with this
#| program.  If not, see <http://www.gnu.org/licenses/>.
#|
#****************************************************************************************
# Set the application name
APPNAME := filespector

# Set the directories
SRCDIR := src
LOCDIR := $(SRCDIR)/languages

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

# Collect all translation files
POFILES := $(wildcard $(LOCDIR)/*.po)
# Build list with machine object translation files.
MOFILES := $(patsubst %.po,%.mo,$(POFILES))
# Build list with installation files of machine object translation files.
MOINSTFILES := $(patsubst $(LOCDIR)/$(APPNAME).%.mo,$(DESTDIR)$(PREFIX)/share/locale/%/LC_MESSAGES/$(APPNAME).mo,$(MOFILES))

# The "all" target for building the application and the machine object translation files.	
.PHONY: all	
all: $(SRCDIR)/$(APPNAME) $(MOFILES)

# Target for building the application	
$(SRCDIR)/$(APPNAME): $(SRCDIR)/$(APPNAME).lpi 
	$(LAZBUILD) --build-mode="$(BUILDMODE)" --widgetset=$(WIDGETSET) $(SRCDIR)/$(APPNAME).lpi 
	
# Pattern rule for building the machine object translation files.	
$(MOFILES): %.mo: %.po
	$(MSGFMT) -o $@ $<

# Target for cleaning up all the files created by the "all" target.
.PHONY: clean
clean:
	rm -f $(SRCDIR)/$(APPNAME)
	rm -f $(SRCDIR)/$(APPNAME).res
	rm -rf $(SRCDIR)/lib
	rm -rf $(LOCDIR)/*.mo	

# Pattern rule for installing the machine object translation files.
$(MOINSTFILES): $(DESTDIR)$(PREFIX)/share/locale/%/LC_MESSAGES/$(APPNAME).mo: $(LOCDIR)/$(APPNAME).%.mo
	install -d -m 755 $(dir $@)
	install -pm 644 $< $@

# Target to install the application. 
.PHONY: install
install: $(MOINSTFILES)
	install -d $(DESTDIR)$(PREFIX)/bin/
	install $(SRCDIR)/$(APPNAME) $(DESTDIR)$(PREFIX)/bin/
	install -d $(DESTDIR)$(PREFIX)/share/$(APPNAME)
	install $(SRCDIR)/$(APPNAME).ico $(DESTDIR)$(PREFIX)/share/$(APPNAME)
	install -d $(DESTDIR)$(PREFIX)/share/applications
	install $(SRCDIR)/$(APPNAME).desktop $(DESTDIR)$(PREFIX)/share/applications

# Target to uninstall the application. 
.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/$(APPNAME)
	rm -f $(DESTDIR)$(PREFIX)/share/$(APPNAME)/$(APPNAME).ico
	rm -f $(DESTDIR)$(PREFIX)/share/applications/$(APPNAME).desktop
	if [ -d "$(DESTDIR)$(PREFIX)/share/$(APPNAME)" ]; then rmdir $(DESTDIR)$(PREFIX)/share/$(APPNAME); fi	
	rm -f $(MOINSTFILES)

