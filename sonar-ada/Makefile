BUILD_DIR := target

MVN            := BUILD_DIR=$(BUILD_DIR) mvn -PalternateBuildDir
MVN_OPTS       := -Xmx512m $(MAVEN_OPTS)
MVN_BUILD_OPTS := -Dmaven.test.skip=true
MVN_TEST_OPTS  :=

.PHONY: build

build:
	MAVEN_OPTS="$(MVN_OPTS)" $(MVN) $(MVN_BUILD_OPTS) package

set-version:
	MAVEN_OPTS="$(MVN_OPTS)" $(MVN) versions:set "-DnewVersion=$(SONAR_ADA_PLUGIN_VERSION)"

test:
	$(MVN) $(MVN_TEST_OPTS) test

clean:
	$(MVN) clean
