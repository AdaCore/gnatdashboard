include shared.mk

DISTRIB_BASE_URL := ~gnatmail/svn-binaries/python/2.7.3/distrib/$(target)
DISTRIB_URL      := ssh.eu.adacore.com:$(DISTRIB_BASE_URL)/python-2.7.3-minimal.tar.gz

PYTHON_MODULES   := flake8 pylint

all: python-distrib

python-distrib: $(BUILD_DIR)/python-2.7.3.tar.gz
	$(MKDIR) $(PYTHON_HOME)
	(cd $(PYTHON_HOME) && tar -xzf $< --strip-components=1)
	$(PY_ENV) easy_install $(PYTHON_MODULES)

$(BUILD_DIR)/python-2.7.3.tar.gz:
	scp $(DISTRIB_URL) $@
