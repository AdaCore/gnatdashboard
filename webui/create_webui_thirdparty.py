#!/usr/bin/env python

import time
from subprocess import call

DATETIME = time.strftime('%Y-%m-%d-%H-%M-%S')
NODE_MODULE_PATH = './node_modules/'
TAR_NAME = 'webui_node_modules_' + DATETIME + '.tar.gz '

TAR_CMD = "tar -zcf " + TAR_NAME + NODE_MODULE_PATH
print("Executing : " + TAR_CMD)
print("Can take a little bit of time...")
call(TAR_CMD.split(' '))
