PLUGIN_VERSION=1.0-SNAPSHOT
SONAR_HOME=/Users/kiwi/tools/sonar-tool/sonar/sonar-3.5.1
MAVEN_OPTS=-Dmaven.test.failure.ignore=true

all:
	mvn $(MAVEN_OPTS) clean install 
	cp target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/
	/Users/kiwi/tools/sonar-tool/sonar/sonar-3.5.1/bin/macosx-universal-64/sonar.sh restart

test:
	mvn $(MAVEN_OPTS) clean test
