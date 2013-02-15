TEST_RESULTS_DIR=$(shell /bin/pwd)/test/result
PLUGIN_DIR=$(shell /bin/pwd)/../sonar-ada-plugin
PLUGIN_VERSION=1.0-SNAPSHOT
PROTOTYPE_DIR=$(shell /bin/pwd)/../prototype

all: build install test

build:
	cd $(PLUGIN_DIR) && mvn clean install -Dmaven.test.skip=true

install:
	cp $(PLUGIN_DIR)/target/sonar-ada-plugin-${PLUGIN_VERSION}.jar ${SONAR_HOME}/extensions/plugins/

test: clean
	cd $(PLUGIN_DIR) && mvn test 
	cd test && python process_testsuite_report.py $(TEST_RESULTS_DIR)

clean:
	cd $(PLUGIN_DIR) && mvn clean 
	 [ ! -z $(TEST_RESULTS_DIR) ] && rm -f $(TEST_RESULTS_DIR)/*
	
packaging:
	mkdir -p basic_driver
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/basic_driver/qualimetrics.py basic_driver/
	# Lib
	mkdir -p basic_driver/lib
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/run.sh basic_driver/lib/
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/create_cp_report.py basic_driver/lib/
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/create_gcov_report.py basic_driver/lib/
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/utils.py basic_driver/lib/
	cp -pf $(PROTOTYPE_DIR)/gps-plugins/qmt/qmt.py basic_driver/lib/


