# Makefile for GNAThub - the GNATdashboard driver.

include version.mk

ifeq ($(OS),Windows_NT)
  IS_WINDOWS := yes
endif

# Build variables
DATE := $(shell date '+%Y%m%d')
YEAR := $(shell date '+%Y')
BUILD_MODE = Production

# Command variables
CP = cp
RM = rm
RMDIR = rm -r
MKDIR = mkdir -p
CHMOD = chmod
INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = ${INSTALL} -m 644
GPRBUILD = gprbuild -p -j0
PYTHON = python
DB2ADA = gnatcoll_db2ada
PYTHON_PREFIX = $(shell $(PYTHON) \
		 -c "from distutils.sysconfig import PREFIX; print(PREFIX)")

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
builddir = $(rootdir)/build
gensrcdir = $(builddir)/gen-src

# Target "gen-db-bindings" generates the following sources at once but Make only
# needs to be aware of one, otherwise parallel processing might fail (eg. -j0):
#   * database.ads
#   * database.adb
#   * database_names.adb
#   * database-orm.ads
#   * database-orm.adb
GENERATED_FILENAMES := gnathub-version.ads database.ads
GENERATED_SOURCES := $(foreach src,$(GENERATED_FILENAMES),$(gensrcdir)/$(src))

.PHONY: $(gensrcdir)/gnathub-version.ads

# Standard targets for users
all: $(builddir)/bin/gnathub

install: $(DESTDIR)$(datadir)/gnathub/python all installdirs
	$(INSTALL_PROGRAM) $(builddir)/bin/gnathub $(DESTDIR)$(bindir)
	(cd "$(rootdir)/share" && tar cf - gnathub) | \
		(cd "$(DESTDIR)$(datadir)" && tar xf -)
	find $(DESTDIR)$(datadir)/gnathub/core -name '*.pyc' -delete
	find $(DESTDIR)$(datadir)/gnathub/extras -name '*.pyc' -delete
	(cd "$(rootdir)/share/examples" && tar cf - gnatdashboard) | \
		(cd "$(DESTDIR)$(datadir)/examples" && tar xf -)

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

clean: $(rootdir)/gnathub.gpr
	gprclean -P $<
	$(RMDIR) $(gensrcdir)
	$(RMDIR) $(builddir)

distclean: clean
	find $(rootdir) -name '*.pyc' -delete
	$(RMDIR) $(rootdir)/testsuite/out

installdirs:
	$(MKDIR) $(DESTDIR)$(bindir)
	$(MKDIR) $(DESTDIR)$(datadir)/gnathub
	$(MKDIR) $(DESTDIR)$(datadir)/examples

# Custom targets for this project
$(builddir)/bin/gnathub: $(rootdir)/gnathub.gpr $(GENERATED_SOURCES)
	$(GPRBUILD) -P $< \
		-XBUILD_DIR="$(builddir)" -XVERSION="$(VERSION)" \
		-XBUILD_MODE="$(BUILD_MODE)" -XGEN_SRC_DIR="$(gensrcdir)"
ifdef IS_WINDOWS
	$(MKDIR) $(builddir)/bin
	$(INSTALL) $(PYTHON_PREFIX)/python39.dll $(builddir)/bin
endif

$(gensrcdir)/gnathub-version.ads: $(rootdir)/templates/gnathub-version.ads.in
	$(MKDIR) $(gensrcdir)
	sed \
		-e 's/@DATE@/$(DATE)/g' \
		-e 's/@YEAR@/$(YEAR)/g' \
		-e 's/@VERSION@/$(VERSION)/g' \
		> $@ < $<

$(gensrcdir)/database%: $(rootdir)/share/gnathub/sql/db.model
	$(MAKE) gen-db-bindings

gen-db-bindings: $(rootdir)/share/gnathub/sql/db.model
	$(MKDIR) $(gensrcdir)
	(cd $(gensrcdir) && \
		$(DB2ADA) -api=Database -orm=Database.ORM -dbmodel=$<)

$(DESTDIR)$(datadir)/gnathub/python:
	if test ! -d "$@"; then \
		$(MKDIR) "$@"; \
		(cd "$(realpath $(dir $(PYTHON_PREFIX)))" && tar cf - $(notdir $(PYTHON_PREFIX))) | \
			(cd "$@" && tar xvf - --strip-component 1) \
	fi
