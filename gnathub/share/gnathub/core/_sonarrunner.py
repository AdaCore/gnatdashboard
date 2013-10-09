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

import GNAThub.utils
import os

from GNAThub import GPSTarget, Log
from GNAThub.utils import OutputParser

from sonarconfig import SonarConfiguration


class SonarRunnerOutputParser(OutputParser):
    """Define custom output parser"""

    def on_stdout(self, text):
        with open(SonarRunner.logs(), 'w+a') as log:
            log.write(text)

    def on_stderr(self, text):
        with open(SonarRunner.logs(), 'w+a') as log:
            log.write(text)


class SonarRunner(GNAThub.Plugin):
    DIR = 'sonar'
    TOOL_NAME = 'Sonar Runner'

    def __init__(self, session):
        super(SonarRunner, self).__init__()

        self.working_dir = os.path.join(GNAThub.root(), self.DIR)

        self.process = GPSTarget(name=self.name,
                                 output_parser='sonarrunneroutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Returns command line for sonar runner execution """

        # Set specific location for Sonar project properties file
        project_settings = os.path.join(self.working_dir,
                                        SonarConfiguration.FILE_NAME)
        sonar_conf = '-Dproject.settings=%s' % project_settings
        return ['sonar-runner', sonar_conf]

    def execute(self):
        status = self.process.execute()

        # if sonar-runner execution has failed
        if status == GNAThub.EXEC_FAIL:
            Log.warn('%s execution returned on failure', self.name)
            Log.warn('See log file: %s' % self.logs())
        else:
            # If sonar-runner execution succeed or did not happened,
            # in last case GPSTarget manage error output so just
            # return status
            return status
