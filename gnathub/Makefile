# Makefile for GNAThub

include config.mk

.PHONY: docs

all: gnathub-bin gnathub-pylib gnathub-plugins

gnathub-bin: $(GENERATED_DIR)/src/gnathub-version.ads
	@echo ":: Building GNAThub driver"
	gprbuild -p -P gnathub.gpr -XBUILD_DIR="$(BUILD_DIR)" -XVERSION="$(VERSION)"
ifeq ($(target),x86-windows)
	$(MKDIR) $(BUILD_DIR)/bin
	cp $(PYTHON_HOME)/python27.dll $(BUILD_DIR)/bin/.
	chmod +x $(BUILD_DIR)/bin/python27.dll
endif

$(GENERATED_DIR)/src/gnathub-version.ads: templates/gnathub-version.ads.in
	$(MKDIR) $(GENERATED_DIR)/src
	cat $< |sed \
		-e 's/@DATE@/$(DATE)/g' \
		-e 's/@YEAR@/$(YEAR)/g' \
		-e 's/@VERSION@/$(VERSION)/g' \
		> $@

gnathub-pylib: python-env
	@echo ":: Building GNAThub's Python API"
	-[ -f $(SETUP_PY_MANIFEST) ] && cat $(SETUP_PY_MANIFEST) |xargs rm -rf
	$(PYTHON) src/lib/setup.py -q build -b "$(BUILD_DIR)" install \
		--record $(SETUP_PY_MANIFEST)

gnathub-plugins:
	@echo ":: Deploying GNAThub's core plug-ins"
	(cd share/gnathub && tar cf - core extras sql) | \
		(cd $(BUILD_DIR)/share/gnathub && tar xf -)

gnathub-api-docs: docs/Makefile
	$(MAKE) -C docs api-doc

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

create-db:
	@echo ":: Generating the GNAThub's database"
	$(MKDIR) src/orm-gen
	cd src/orm-gen && gnatcoll_db2ada -dbtype=sqlite -dbname=gnathub.db \
		-dbmodel=$(TOP)/share/gnathub/sql/db.model -createdb

generate-orm:
	@echo ":: Generating the GNAThub's database API"
	$(MKDIR) src/orm-gen
	cd src/orm-gen && gnatcoll_db2ada -api=Database -orm=ORM \
		-dbmodel=$(TOP)/share/gnathub/sql/db.model

python-env: $(PYTHON_HOME)

$(PYTHON_HOME):
	@echo "Error: no Python distribution installed at:"
	@echo ""
	@echo "  $(PYTHON_HOME)"
	@echo ""
	@echo "Please refer to the README file for the build procedure"

check-style:
	@echo ":: Style-checking GNAThub's Python API"
	$(PY_ENV) flake8 src/lib/GNAThub
	$(PY_ENV) flake8 share/gnathub/{core,extras,sql}

tests: all
	@echo ":: Running the GNAThub testsuite"
	$(PY_ENV) testsuite/testsuite.py --colorize --with-diff

tests-coverage:
	BUILD_MODE=Coverage $(MAKE) all tests analyse-gcov

analyse-gcov:
	find . -name '*.gcda' > $(BUILD_DIR)/obj/gcov_input.txt
	cd $(BUILD_DIR)/obj && gcov @gcov_input.txt > gcov_run.log

clean: gnathub.gpr
	gprclean -q -P $<
	find . -name '*.pyc' -exec rm -f {} \;
	$(RM) gnathub.gpr
	$(RM) src/gnathub/gnathub-version.ads
	$(RMDIR) $(BUILD_DIR)/obj
	$(RMDIR) $(BUILD_DIR)/lib.*
	$(RMDIR) $(BUILD_DIR)/docs

distclean: clean
	$(RMDIR) $(BUILD_DIR)
	$(RMDIR) testsuite/out

package: all
	$(RMDIR) $(PKG_DIR)
	$(MKDIR) $(PKG_DIR)
	(tar cf - COPYING3) | (cd $(PKG_DIR) && tar xf -)
	(cd $(BUILD_DIR) && tar cf - bin share) | (cd $(PKG_DIR) && tar xf -)
	$(MKDIR) $(PKG_DIR)/share/examples
	(cd share/examples && tar cf - gnatdashboard) | (cd $(PKG_DIR)/share/examples && tar xf -)

install: package
	$(RSYNC) $(PKG_DIR) $(INSTALL_DIR)
