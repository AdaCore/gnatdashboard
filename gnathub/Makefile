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

include Makefile.conf

all: gnathub-bin gnathub-pylib gnathub-plugins

gnathub-bin: gnathub.gpr src/gnathub/gnathub-version.ads
	BUILD_DIR="$(BUILD_DIR)" gprbuild -q -p -P $<

gnathub.gpr: gnathub.gpr.in
	cat $< |sed -e 's/@VERSION@/$(VERSION)/g' > $@

src/gnathub/gnathub-version.ads: src/gnathub/gnathub-version.ads.in
	cat $< |sed \
		-e 's/@DATE@/$(DATE)/g' \
		-e 's/@YEAR@/$(YEAR)/g' \
		-e 's/@VERSION@/$(VERSION)/g' \
		> $@

gnathub-pylib: python-env
	$(PYTHON) src/lib/setup.py -q build -b "$(BUILD_DIR)" install

gnathub-plugins:
	(cd share/gnathub && tar cf - core extras sql) | \
		(cd $(BUILD_DIR)/share/gnathub && tar xf -)

createdb:
	# Create the database from the schema
	$(MKDIR) src/orm-gen
	cd src/orm-gen && gnatcoll_db2ada -dbtype=sqlite -dbname=gnathub.db \
		-dbmodel=$(TOP)/share/gnathub/sql/db.model -createdb

generate-orm:
	# Generate the Ada API
	$(MKDIR) src/orm-gen
	cd src/orm-gen && gnatcoll_db2ada -api=Database -orm=ORM \
		-dbmodel=$(TOP)/share/gnathub/sql/db.model

python-env: $(PYTHON_HOME)

$(PYTHON_HOME):
	virtualenv $@
	$(PY_ACTIVATE) && pip install $(PY_MODULES)

check-style:
	$(PY_ACTIVATE) && flake8 src/lib/GNAThub
	$(PY_ACTIVATE) && flake8 share/gnathub/{core,extras,sql}
	$(PY_ACTIVATE) && pylint \
		--rcfile=t/etc/pylintrc \
		--output-format=parseable \
		--reports=n GNAThub share/gnathub/{core,extras,sql}/*.py

tests: all
	@python t/testsuite.py --colorize --with-diff

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

package: all
	$(RMDIR) $(PKG_DIR)
	$(MKDIR) $(PKG_DIR)
	(tar cf - COPYING3) | (cd $(PKG_DIR) && tar xf -)
	(cd $(BUILD_DIR) && tar cf - bin share) | (cd $(PKG_DIR) && tar xf -)

install: package
	(cd $(PKG_DIR) && tar cf - *) | (cd $(INSTALL_DIR) && tar xf -)
