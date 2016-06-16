# Makefile for GNATdashboard

# Command variables
CP = cp
MKDIR = install -d

# General conventions for Makefiles
SHELL = /bin/sh

.SUFFIXES:

# Common prefix for installation directories.
# NOTE: This directory must exist when you start the install.
prefix = /usr/local
datarootdir = $(prefix)/share
docdir = $(datarootdir)/doc/gnatdashboard
pdfdir = $(docdir)/pdf
htmldir = $(docdir)/html

mkfile := $(abspath $(lastword $(MAKEFILE_LIST)))
rootdir := $(patsubst %/,%,$(dir $(mkfile)))
builddir = $(rootdir)/build
gnathubdir = $(rootdir)/gnathub

MAKE_DOC = $(MAKE) -C docs builddir=$(builddir) GNATHUBDIR=$(gnathubdir)

# Generate both HTML and PDF documents
all: gnathub-api-docs docs-html docs-pdf

gnathub-api-docs: docs/Makefile
	$(MAKE_DOC) api-doc
	$(MAKE_DOC) plugins-doc

# Generate the HTML documentation
docs-html: gnathub-api-docs
	$(MAKE_DOC) html

# Generate the PDF documentation
docs-pdf: gnathub-api-docs
	$(MAKE_DOC) latexpdf

installdirs:
	$(MKDIR) $(docdir)
	$(MKDIR) $(pdfdir)
	$(MKDIR) $(htmldir)

install-html: docs-html installdirs
	(cd $(builddir)/docs && tar cf - html) | \
		(cd $(htmldir) && tar xf - --strip-components 1)

install-pdf: docs-pdf installdirs
	$(CP) $(builddir)/docs/latex/GNATdashboard.pdf $(pdfdir)

install: install-html install-pdf
