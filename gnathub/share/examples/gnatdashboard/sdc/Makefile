all: build run gcov gnathub gnathub_report

build:
	gprbuild -p -P sdc

run: build
	obj/sdc < input.txt

gcov: run
	cd obj && \
	gcov input instructions screen_output sdc stack tokens values-operations values

gnathub:
	gnathub -Psdc

gnathub_report: gnathub
	gnathub -Psdc --exec simple_dump.py

clean:
	gprclean -q -P sdc
