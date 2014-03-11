#
# Download and deploy an in-house Python distribution.
#
# This file should be excluded from the source package. It is the user's
# responsibility to download and deploy a Python distribution if he wants to
# build GNAThub himself.
#

include shared.mk

SILENCED := > /dev/null 2>&1

ifeq ($(V),1)
  SILENCED :=
endif

DISTRIB_BASE_URL := ~gnatmail/svn-binaries/python/2.7.3/distrib/$(target)
DISTRIB_URL      := ssh.eu.adacore.com:$(DISTRIB_BASE_URL)/python-2.7.3-minimal.tar.gz

GIT_OPENDO_FORGE := https://forge.open-do.org/anonscm/git
GNATPYTHON_REPOSITORY := $(GIT_OPENDO_FORGE)/gnatpython/gnatpython.git

PYTHON_MODULES := flake8 pylint pygments pyyaml

all: python-distrib

python-distrib: $(BUILD_DIR)/python-2.7.3.tar.gz $(BUILD_DIR)/gnatpython
	@echo ":: Deploying Python distribution"
	$(MKDIR) $(PYTHON_HOME)
	(cd $(PYTHON_HOME) && tar -xzf $< --strip-components=1) $(SILENCED)
	@echo ":: Installing required modules ($(PYTHON_MODULES))"
	$(PY_ENV) easy_install $(PYTHON_MODULES) $(SILENCED)
	(cd $(BUILD_DIR)/gnatpython && $(PY_ENV) python setup.py install) $(SILENCED)

$(BUILD_DIR)/gnatpython:
	@echo ":: Fetching GNATpython (public)"
	git clone $(GNATPYTHON_REPOSITORY) $@ $(SILENCED)

$(BUILD_DIR)/python-2.7.3.tar.gz:
	@echo ":: Fetching custom Python distribution"
	scp $(DISTRIB_URL) $@

clean:
	$(RM) $(BUILD_DIR)/python-2.7.3.tar.gz
	$(RMDIR) $(BUILD_DIR)/gnatpython

distclean: clean
	$(RMDIR) $(PYTHON_HOME)
