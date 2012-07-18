PLUGIN_VERSION=1.0-SNAPSHOT
SONAR_HOME=/Users/kiwi/tools/sonar-tool/sonar/sonar-3.1.1
PLATFORM=macosx-universal-64

all:
	mvn clean install 
	cp target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/
	${SONAR_HOME}/bin/${PLATFORM}/sonar.sh stop
	${SONAR_HOME}/bin/${PLATFORM}/sonar.sh start
