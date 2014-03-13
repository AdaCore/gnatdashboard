##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                        Copyright (C) 2013, AdaCore                       ##
##                                                                          ##
## The QM is free software; you can redistribute it  and/or modify it       ##
## under terms of the GNU General Public License as published by the Free   ##
## Software Foundation; either version 3, or (at your option) any later     ##
## version.  The QM is distributed in the hope that it will be useful,      ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License  for more details. You  should  have  received a copy of the GNU ##
## General Public License  distributed with the QM; see file COPYING3. If   ##
## not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite ##
## 330, Boston, MA 02111-1307, USA.                                         ##
##                                                                          ##
##############################################################################

include config.mk

all: gnathub-bin gnathub-pylib gnathub-plugins

gnathub-bin: gnathub.gpr src/gnathub/gnathub-version.ads
	@echo ":: Building GNAThub driver"
	gprbuild -p -P $< -XBUILD_DIR="$(BUILD_DIR)"
ifeq ($(target),x86-windows)
	$(MKDIR) $(BUILD_DIR)/bin
	cp $(PYTHON_HOME)/python27.dll $(BUILD_DIR)/bin/.
	chmod +x $(BUILD_DIR)/bin/python27.dll
endif

gnathub.gpr: gnathub.gpr.in
	cat $< |sed -e 's/@VERSION@/$(VERSION)/g' > $@

src/gnathub/gnathub-version.ads: src/gnathub/gnathub-version.ads.in
	cat $< |sed \
		-e 's/@DATE@/$(DATE)/g' \
		-e 's/@YEAR@/$(YEAR)/g' \
		-e 's/@VERSION@/$(VERSION)/g' \
		> $@

gnathub-pylib: python-env
	@echo ":: Building GNAThub's Python API"
	$(PYTHON) src/lib/setup.py -q build -b "$(BUILD_DIR)" install

gnathub-plugins:
	@echo ":: Deploying GNAThub's core plug-ins"
	(cd share/gnathub && tar cf - core extras sql) | \
		(cd $(BUILD_DIR)/share/gnathub && tar xf -)

createdb:
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

distclean: clean
	$(RMDIR) $(BUILD_DIR)
	$(RMDIR) testsuite/out

package: all
	$(RMDIR) $(PKG_DIR)
	$(MKDIR) $(PKG_DIR)
	(tar cf - COPYING3) | (cd $(PKG_DIR) && tar xf -)
	(cd $(BUILD_DIR) && tar cf - bin share) | (cd $(PKG_DIR) && tar xf -)

install: package
	(cd $(PKG_DIR) && tar cf - *) | (cd $(INSTALL_DIR) && tar xf -)
