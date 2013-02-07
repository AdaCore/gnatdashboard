PLUGIN_VERSION=1.0-SNAPSHOT
SONAR_HOME=/Users/kiwi/tools/sonar-tool/sonar/sonar-3.3.2
MAVEN_OPTS=-Dmaven.test.failure.ignore=true

all:
	mvn $(MAVEN_OPTS) clean install 
	cp target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/
	sonar.sh restart

test:
	mvn $(MAVEN_OPTS) clean test
