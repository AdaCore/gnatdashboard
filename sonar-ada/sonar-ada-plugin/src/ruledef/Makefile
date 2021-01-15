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
	$(SONAR_ADA_PLUGIN_RESOURCES)/gnatcoverage.xml \
	$(SONAR_ADA_PLUGIN_RESOURCES)/spark2014.xml

all: ada_rules_profile

ada_rules_profile: $(SONAR_ADA_PLUGIN_RESOURCES)/default-profile.xml

ada_rules_profile_definition:
	python $(TOP)/scripts/generate_ada_default_profile.py $(RULES_DEFINITIONS) >  \
         $(SONAR_ADA_PLUGIN_RESOURCES)/default-profile.xml

codepeer_rules_definition:
	codepeer --list-categories 2>&1 | python $(TOP)/scripts/generate_codepeer_rules.py > \
         $(SONAR_ADA_PLUGIN_RESOURCES)/codepeer.xml

#   Technical debts are listed for Codepeer rules --list-categories
#   (starting from 20190610 wavefronts)
codepeer_rules_definition_with_technical_debt:
	codepeer --list-categories 2>&1 | python $(TOP)/scripts/generate_codepeer_rules_with_debt.py > \
         $(SONAR_ADA_PLUGIN_RESOURCES)/codepeer.xml

#  This script is usable for all previous versions of GNATcheck without technical debt infos listed
gnatcheck_rules_definition:
	gnatcheck -h 2>&1 | python $(TOP)/scripts/generate_gnatcheck_rules.py > \
         $(SONAR_ADA_PLUGIN_RESOURCES)/gnatcheck.xml

#   Technical debts are listed for gnatcheck rules on -h output
#   (starting from 20190604 versions)
gnatcheck_rules_definition_with_technical_debt:
	gnatcheck -h 2>&1 | python $(TOP)/scripts/generate_gnatcheck_rules_with_debt.py > \
         $(SONAR_ADA_PLUGIN_RESOURCES)/gnatcheck.xml

#  This script is usable for gnatprove without technical debt infos listed
spark_rules_definition:
	gnatprove --list-categories 2>&1 | python $(TOP)/scripts/generate_spark_rules.py > \
         $(SONAR_ADA_PLUGIN_RESOURCES)/spark2014.xml

$(BUILD_DIR)/$(CODEPEER_DOC):
	mkdir -p $(BUILD_DIR)
	rm -f $@
	svn export $(CODEPEER_SVN_USERS_GUIDE)/$(CODEPEER_DOC) $@

clean:
	rm -f $(BUILD_DIR)/$(CODEPEER_DOC)
