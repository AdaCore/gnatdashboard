PLUGIN_VERSION=1.0-SNAPSHOT
SONAR_HOME=/Users/kiwi/tools/sonar-tool/sonar/sonar-3.3.2
all:
	mvn clean install 
	cp target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/
	sh /Users/kiwi/tools/sonar-tool/sonar/sonar-3.3.2/bin/macosx-universal-64/sonar.sh restart

test:
	mvn clean test
