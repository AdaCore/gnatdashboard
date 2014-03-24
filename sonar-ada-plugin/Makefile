BUILD_DIR := target

MVN            := BUILD_DIR=$(BUILD_DIR) mvn -PalternateBuildDir
MVN_BUILD_OPTS := -Dmaven.test.skip=true
MVN_TEST_OPTS  :=

build:
	$(MVN) $(MVN_BUILD_OPTS) package

test:
	$(MVN) $(MVN_TEST_OPTS) test

clean:
	$(MVN) clean
