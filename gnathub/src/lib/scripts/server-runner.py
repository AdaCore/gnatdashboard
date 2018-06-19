"""Execute GNAThub WEB server script
"""

# GNAThub (GNATdashboard)
# Copyright (C) 2018, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

import os
import inspect
import logging

import GNAThub
from GNAThub import Console

# Create this script logger
__file__ = inspect.getfile(inspect.currentframe())
MODULE, _ = os.path.splitext(os.path.basename(__file__))
LOG = logging.getLogger(MODULE)

# Define default path to server.py script
DEFAULT_SCRIPT_PATH = GNAThub.engine_repository()
SCRIPT_NAME = 'server.py'

# Determine script path and check is different of default value
script_path = DEFAULT_SCRIPT_PATH
#  TO DO : Add handling when path is given via --server-dir

if not os.path.exists(script_path):
    repo_msg = script_path + ' repository does not exist'
    Console.error(repo_msg, prefix=MODULE)

else:
    msg = 'load script from ' + script_path + ' repository'
    Console.info(msg, prefix=MODULE)

    # Build server script full path
    server_script_path = os.path.join(script_path, SCRIPT_NAME)
    if os.path.exists(server_script_path):
        if os.path.isfile(server_script_path):
            try:
                msg_exec = 'execute server script ' + SCRIPT_NAME
                Console.info(msg_exec, prefix=MODULE)
                execfile(server_script_path)
            except Exception as why:
                msg_excpt = 'failed to load script: ' + server_script_path
                msg_err = msg_excpt + ' ' + str(why)
                Console.info(msg_excpt, prefix=MODULE)
                Console.error(msg_err, prefix=MODULE)

    else:
        file_msg = server_script_path + ' server script does not exist'
        Console.error(file_msg, prefix=MODULE)
