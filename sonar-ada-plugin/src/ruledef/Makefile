# Makefile for generating sonar-ada-plugin rules definitions and profiles

ME  := $(word $(words $(MAKEFILE_LIST)), $(MAKEFILE_LIST))
TOP := $(shell cd $(dir $(ME)); pwd)

BUILD_DIR                  := $(TOP)/../../build
SONAR_ADA_PLUGIN_RESOURCES := $(TOP)/../main/resources

CODEPEER_SVN_REPOSITORY    := svn+ssh://svn.us.adacore.com/Dev/trunk/codepeer
CODEPEER_USERS_GUIDE       := share/doc/users_guide
CODEPEER_DOC               := messages_and_annotations.rst
CODEPEER_SVN_USERS_GUIDE   := $(CODEPEER_SVN_REPOSITORY)/$(CODEPEER_USERS_GUIDE)

RULES_DEFINITIONS := $(SONAR_ADA_PLUGIN_RESOURCES)/codepeer.xml \
	$(SONAR_ADA_PLUGIN_RESOURCES)/gnatcheck.xml \
	$(SONAR_ADA_PLUGIN_RESOURCES)/gnatprove.xml \
	$(SONAR_ADA_PLUGIN_RESOURCES)/gnatcoverage.xml

all: ada_rules_profile

ada_rules_profile: $(SONAR_ADA_PLUGIN_RESOURCES)/default-profile.xml

codepeer_rules_definition: $(SONAR_ADA_PLUGIN_RESOURCES)/codepeer.xml

$(SONAR_ADA_PLUGIN_RESOURCES)/default-profile.xml: $(RULES_DEFINITIONS)
	PYTHONPATH=$(TOP)/$(PYTHONPATH) \
		python $(TOP)/scripts/generate-ada-default-profile -o $@ $^

$(SONAR_ADA_PLUGIN_RESOURCES)/codepeer.xml: $(BUILD_DIR)/$(CODEPEER_DOC)
	PYTHONPATH=$(TOP)/$(PYTHONPATH) \
		python $(TOP)/scripts/generate-codepeer-rules-definition -o $@ $<

$(BUILD_DIR)/$(CODEPEER_DOC):
	mkdir -p $(BUILD_DIR)
	svn export $(CODEPEER_SVN_USERS_GUIDE)/$(CODEPEER_DOC) $@
