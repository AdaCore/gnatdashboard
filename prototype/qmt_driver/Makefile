all:
	# -m switch is so that gnatmake does not use timestamps but checksums$
	# for dependencies (otherwise since we regenerate the files every time$
	# we also recompile every time)$
	gnatmake -gnat2012 -q -g -m -Pqualimetrics.gpr

create:
	# Create the database from the schema
	cd obj/gnatcoll_sqlite && gnatcoll_db2ada -dbtype=sqlite -dbname=qualimetrics.db -dbmodel=../../dbschema.txt -createdb

orm:
	# Generate the Ada API
	cd obj/gnatcoll_sqlite && gnatcoll_db2ada -api=Database -orm=ORM -dbmodel=../../dbschema.txt
