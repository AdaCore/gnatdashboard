workdir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
VERSION := $(shell cat $(dir $(abspath $(workdir)))/VERSION)
