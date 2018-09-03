PROJECT = gcov_multi_object_dir

all: build

build:
	gprbuild -p -P $(PROJECT)

run: run
	cd obj && \
	gcov \
		input instructions screen_output sdc stack \
		tokens values-operations values            \
		obj2/src2 obj2/obj3/src3 obj2/obj4/src4    \
		> /dev/null && \
	mv src2.adb.gcov obj2 && \
	mv src3.adb.gcov obj2/obj3 && \
	mv src4.adb.gcov obj2/obj4

clean:
	gprclean -q -P $(PROJECT)
