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

# Build variables
DATE      := $(shell date '+%Y%m%d')
YEAR      := $(shell date '+%Y')
VERSION   := $(shell cat $(TOP)/VERSION)

BUILD_DIR := $(TOP)/.build
PKG_DIR   := $(TOP)/gnatdashboard-$(VERSION)-$(target)
DOCS_DIR  := $(PKG_DIR)/share/doc/gnatdashboard

# Useful aliases
MKDIR := mkdir -p
RM    := rm -f
RMDIR := rm -rf
CP    := cp -pr
RSYNC := rsync -a --delete
