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

.PHONY: tests

all: gnathub

gnathub:
	gprbuild -p -P $(PROJECT).gpr
	$(RMDIR) $(BUILD_DIR)/share
	$(LN) $(TOP)/share $(TOP)/build/share

create:
	# Create the database from the schema
	$(MKDIR) src-gen
	cd src-gen && gnatcoll_db2ada -dbtype=sqlite -dbname=$(PROJECT).db \
			-dbmodel=$(TOP)/share/gnathub/scripts/database-schema.txt -createdb

orm:
	# Generate the Ada API
	$(MKDIR) src-gen
	cd src-gen && gnatcoll_db2ada -api=Database -orm=ORM \
			-dbmodel=$(TOP)/share/gnathub/scripts/database-schema.txt

lint:
	flake8 $(TOP)/share/gnathub/{lib/GNAThub,core,scripts}
	PYTHONPATH=$(TOP)/share/gnathub/lib pylint \
			--rcfile=$(TOP)/tests/etc/pylintrc \
			--output-format=parseable \
			--reports=n GNAThub \
			$(TOP)/share/gnathub/core/*.py \
			$(TOP)/share/gnathub/scripts/*.py

tests:
	cd tests && python testsuite.py

tests-coverage:
	BUILD_MODE=Coverage $(MAKE) all
	cd tests && python testsuite.py
	$(MAKE) analyse-gcov

analyse-gcov:
	find $(TOP) -name '*.gcda' > $(TOP)/build/obj/gcov_input.txt
	cd $(TOP)/build/obj && gcov @gcov_input.txt > gcov_run.log

clean:
	gprclean -q -P $(PROJECT).gpr
	find . -name '*.pyc' -exec rm -f {} \;
	$(RMDIR) build/obj

distclean: clean
	$(RMDIR) build

install:
	$(MKDIR) $(INSTALL_PREFIX)
	cp COPYING3 $(INSTALL_PREFIX)
	$(MKDIR) $(INSTALL_PREFIX)/bin
	$(MKDIR) $(INSTALL_PREFIX)/share
	(cd $(BUILD_DIR) && tar cf - bin) | (cd $(INSTALL_PREFIX) && tar xf -)
	(cd $(TOP)/share && tar cf - --exclude='*.pyc' $(PROJECT)) | (cd $(INSTALL_PREFIX)/share && tar xf -)

distrib: $(PROJECT)
	$(RMDIR) $(DISTRIB_PREFIX)
	PREFIX=$(DISTRIB_PREFIX) $(MAKE) install
