##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                        Copyright (C) 2013, AdaCore                       ##
##                                                                          ##
## The QM is free software; you can redistribute it  and/or modify it       ##
## under terms of the GNU General Public License as published by the Free   ##
## Software Foundation; either version 3, or (at your option) any later     ##
## version.  The QM is distributed in the hope that it will be useful,      ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License  for more details. You  should  have  received a copy of the GNU ##
## General Public License  distributed with the QM; see file COPYING3. If   ##
## not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite ##
## 330, Boston, MA 02111-1307, USA.                                         ##
##                                                                          ##
##############################################################################

# Guess the architecture

gcc_triplet := $(shell gcc -dumpmachine)
machine     := $(subst -, , $(gcc_triplet))

target := unknown
arch   := $(word 1, $(machine))

ifeq ($(words $(machine)), 2)
  osys := $(word 2, $(machine))
else
  osys := $(word 3, $(machine))
endif

ifeq ($(filter-out mingw%, $(osys)),)
  target := x86-windows
endif

ifeq ($(filter-out darwin%, $(osys)),)
  target := x86_64-darwin
endif

ifeq ($(filter-out linux%, $(osys)),)
  ifeq ($(filter-out x86_64, $(arch)),)
    target := x86_64-linux
  else
    target := x86-linux
  endif
endif

# Makefile variables

ME  := $(word $(words $(MAKEFILE_LIST)), $(MAKEFILE_LIST))
TOP := $(shell cd $(dir $(ME)); pwd)

BUILD_DIR   := $(TOP)/build
PKG_DIR     := $(TOP)/gnathub-$(target)
PYTHON_HOME := $(BUILD_DIR)/share/gnathub/python
INSTALL_DIR := ${PREFIX}

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

# Useful aliases

MKDIR := mkdir -p
RM    := rm -f
RMDIR := rm -rf
CP    := cp -pr
RSYNC := rsync -a --delete

# Build variables

DATE    := $(shell date '+%Y%m%d')
YEAR    := $(shell date '+%Y')
VERSION := $(shell cat $(TOP)/VERSION)
