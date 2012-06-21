PLUGIN_VERSION=1.0-SNAPSHOT
SONAR_HOME=/usr/local/sonar/sonar-3.0/
PLATEFORM=macosx-universal-64

all:
	mvn clean install 
	cp target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/
	${SONAR_HOME}/bin/${PLATEFORM}/sonar.sh stop
	${SONAR_HOME}/bin/${PLATEFORM}/sonar.sh start
