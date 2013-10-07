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

all: $(PROJECT)

$(PROJECT):
	-@mkdir -p $(TOP)/build
	-@rm -f $(TOP)/build/share
	-@ln -s $(TOP)/share $(TOP)/build/share
	gprbuild -p -P $(PROJECT).gpr

create:
	# Create the database from the schema
	mkdir -p src-gen
	cd src-gen && gnatcoll_db2ada -dbtype=sqlite -dbname=$(PROJECT).db -dbmodel=../../dbschema.txt -createdb

orm:
	# Generate the Ada API
	mkdir -p src-gen
	cd src-gen && gnatcoll_db2ada -api=Database -orm=ORM -dbmodel=../../dbschema.txt

tests:
	-@cd tests && python testsuite.py

clean:
	gprclean -q -P $(PROJECT).gpr
	find . -name '*.pyc' -exec rm -f {} \;
	rm -rf build/obj

distclean: clean
	rm -rf build

install: $(PROJECT)
	mkdir -p $(INSTALL_PREFIX)
	cp COPYING3 $(INSTALL_PREFIX)
	mkdir -p $(INSTALL_PREFIX)/bin
	mkdir -p $(INSTALL_PREFIX)/share
	(cd build && tar cf - bin) | (cd $(INSTALL_PREFIX) && tar xf -)
	(cd share && tar cf - --exclude='*.pyc' $(PROJECT)) | (cd $(INSTALL_PREFIX)/share && tar xf -)

distrib: $(PROJECT)
	rm -rf $(DISTRIB_PREFIX)
	PREFIX=$(DISTRIB_PREFIX) $(MAKE) install
