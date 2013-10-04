# Copyright (C) AdaCore, 2013

include Makefile.conf

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

test:
	cd tests && python testsuite.py

clean:
	gprclean -q -P $(PROJECT).gpr
	find . -name '*.pyc' -exec rm -f {} \;
	rm -rf build/obj

distclean: clean
	rm -rf build

distrib: $(PROJECT)
	rm -rf $(DISTRIB_PREFIX)
	mkdir -p $(DISTRIB_PREFIX)
	cp COPYING3 $(DISTRIB_PREFIX)
	mkdir -p $(DISTRIB_PREFIX)/bin
	mkdir -p $(DISTRIB_PREFIX)/share
	(cd build && tar cf - bin) | (cd $(DISTRIB_PREFIX) ; tar xf -)
	(cd share && tar cf - --exclude='*.pyc' $(PROJECT)) | (cd $(DISTRIB_PREFIX)/share && tar xf -)
