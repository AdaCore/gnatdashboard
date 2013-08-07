prefix = ${PREFIX}
MKDIR  = mkdir -p

all:
	gprbuild -Pqualimetrics.gpr

create:
	# Create the database from the schema
	cd obj/gnatcoll_sqlite && gnatcoll_db2ada -dbtype=sqlite -dbname=qualimetrics.db -dbmodel=../../dbschema.txt -createdb

orm:
	# Generate the Ada API
	cd obj/gnatcoll_sqlite && gnatcoll_db2ada -api=Database -orm=ORM -dbmodel=../../dbschema.txt

test:
	cd testsuite/ && python testsuite.py

clean:
	gprclean -Pqualimetrics.gpr && rm share/qualimetrics/core/qmt_api/*.pyc

install:
	$(MKDIR) $(prefix)/bin/
	$(MKDIR) $(prefix)/share/
	(tar cf - bin) | (cd $(prefix) ; tar xf -)
	(cd share ; tar cf - qualimetrics) | (cd $(prefix)/share ; tar xf -)
