include ../common.mk

# Build variable
PYTHON_HOME := $(BUILD_DIR)/share/gnathub/python
INSTALL_DIR := ${PREFIX}

# Directory in which the Makefile generates files needed for compilation (eg.
# gnathub-version.ads and gnathub_shared.gpr).
GENERATED_DIR := $(BUILD_DIR)/gnathub-gen

# Python module manifest. Record the list of file installed so that they can be
# removed prior any re-installation. This avoid issues when removing files.
SETUP_PY_MANIFEST := $(BUILD_DIR)/GNAThub.MANIFEST

ifeq ($(target),x86-windows)
  PYTHON         := $(PYTHON_HOME)/python
  PYTHON_BIN_DIR := $(PYTHON_HOME)/Scripts
else
  PYTHON         := $(PYTHON_HOME)/bin/python
  PYTHON_BIN_DIR := $(PYTHON_HOME)/bin
endif

PY_ENV := PATH="$(PYTHON_BIN_DIR):$(PATH)"; export PATH;
