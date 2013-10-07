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

import GPS
import os
import GNAThub.utils
import ConfigParser

from GNAThub import GPSTarget, Log
from GNAThub import utils

from GNAThub.utils import OutputParser, create_parser
from GNAThub.db import Rule, Message
from GNAThub import Session
from GNAThub import dao

from sonarconfig import SonarConfiguration

from xml.etree import ElementTree

## GnatmetricOutputParser #####################################################
##
class SonarrunnerOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Sonarrunner.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Sonarrunner.get_log_file_path(), 'w+a') as log:
            log.write(text)


## Sonarrunner ################################################################
##
class Sonarrunner(GNAThub.Plugin):
    LOG_FILE_NAME='sonar-runner.log'
    DIR='sonar'

    def __init__ (self, session):
        super(Sonarrunner, self).__init__('Sonar Runner')
        self.working_dir = os.path.join(GNAThub.utils.get_qmt_root_dir(), self.DIR)
        self.process = GPSTarget(name=self.name,
                                 output_parser='sonarrunneroutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Return command line for sonar runner execution """
        # Set specific location for Sonar project properties file
        sonar_conf = '-Dproject.settings=%s' % os.path.join(self.working_dir,
                                                SonarConfiguration.FILE_NAME)
        return ['sonar-runner', sonar_conf]

    def execute(self):
        status = self.process.execute()

        # if sonar-runner execution has failed
        if status  == GNAThub.EXEC_FAIL:
            Log.warn('Sonar runner execution returned on failure')
            Log.warn('For more details, see log file: %s' % self.get_log_file_path())
        else:
            # If sonar-runner execution succeed or did not happened,
            # in last case GPSTarget manage error output so just
            # return status
            return status

