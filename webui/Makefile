# Makefile for WebUI - the GNATdashboard Web Interface.

# General conventions for Makefiles
SHELL = /bin/sh

.SUFFIXES:

# Common prefix for installation directories.
# NOTE: This directory must exist when you start the install.
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
libexecdir = $(exec_prefix)/libexec
datarootdir = $(prefix)/share
datadir = $(datarootdir)

mkfile := $(abspath $(lastword $(MAKEFILE_LIST)))
rootdir := $(patsubst %/,%,$(dir $(mkfile)))
srcdir = $(rootdir)/src
builddir = $(rootdir)

# Command variables
CP = cp
RM = rm
RMDIR = rm -r
MKDIR = install -d
CHMOD = chmod
INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = ${INSTALL} -m 644
NPM = npm
PYTHON = python

BUILD_MODE = dev
SOURCES := $(shell find $(srcdir) \
	-name '*.ts' -or -name '*.scss' -or -name '*.html')

.PHONY:

# Standard targets for users
all: $(builddir)/dist/index.html

install: all installdirs
	(cd $(builddir) && tar cf - dist) | \
		(cd "$(DESTDIR)$(datadir)/gnathub/webui" && tar xf - --strip-components 1)

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

test: $(rootdir)/package.json
	$(NPM) run test

lint: $(rootdir)/package.json
	$(NPM) run lint

karma: $(rootdir)/package.json
	$(NPM) run karma

clean:
	-$(RMDIR) $(rootdir)/typings
	-$(RMDIR) $(rootdir)/coverage

distclean: clean
	-$(RMDIR) $(builddir)/dist
	-$(RMDIR) $(rootdir)/node_modules

installdirs:
	$(MKDIR) $(DESTDIR)$(bindir)
	$(MKDIR) $(DESTDIR)$(datadir)/gnathub
	$(MKDIR) $(DESTDIR)$(datadir)/gnathub/webui

# Custom targets for this project
$(builddir)/dist/index.html: $(rootdir)/package.json $(SOURCES) \
		$(srcdir)/index.html $(rootdir)/node_modules/.installed
	BUILD_DIR=$(builddir) $(NPM) run build:dev

install-node-modules: $(rootdir)/node_modules/.installed

thirdparty: $(rootdir)/create_webui_thirdparty.py
	$(PYTHON) $(rootdir)/create_webui_thirdparty.py

$(rootdir)/node_modules/.installed: $(rootdir)/package.json
	(cd $(rootdir) && $(NPM) install && touch $@)
