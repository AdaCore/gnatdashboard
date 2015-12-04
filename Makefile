# Makefile for GNATdashboard

include common.mk

.PHONY: docs

all: docs

gnathub-api-docs: docs/Makefile
	$(MAKE) -C docs api-doc
	$(MAKE) -C docs plugins-doc

# Generate both HTML and PDF documents in BUILD_DIR and install them in DOCS_DIR
docs: gnathub-api-docs docs-html docs-pdf
	$(MKDIR) $(DOCS_DIR)/pdf $(DOCS_DIR)/html
	$(CP) $(BUILD_DIR)/docs/latex/GNATdashboard.pdf $(DOCS_DIR)/pdf
	$(RSYNC) $(BUILD_DIR)/docs/html $(DOCS_DIR)

# Generate the HTML documentation in BUILD_DIR
docs-html: gnathub-api-docs
	$(MAKE) -C docs html

# Generate the PDF documentation in BUILD_DIR
docs-pdf: gnathub-api-docs
	$(MAKE) -C docs latexpdf

install: all
	$(MKDIR) $(PKG_DIR)
	(tar cf - COPYING3) | (cd $(PKG_DIR) && tar xf -)
	$(MKDIR) $(PKG_DIR)/share/doc
	(cd `dirname $(DOCS_DIR)` && tar cf - `basename $(DOCS_DIR)`) \
		| (cd $(PKG_DIR)/share/doc && tar xf -)
