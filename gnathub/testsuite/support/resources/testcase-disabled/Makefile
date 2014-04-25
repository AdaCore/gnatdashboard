PROJECT := simple

all: build

build:
	gprbuild -p -P$(PROJECT)

run:
	(cd obj && ./$(PROJECT) && gcov $(PROJECT) f > /dev/null 2>&1)

clean:
	gprclean -P$(PROJECT)
