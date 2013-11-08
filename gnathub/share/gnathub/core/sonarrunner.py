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

"""GNAThub plug-in for the SonarQube Runner command-line tool.

It exports the SonarRunner Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import GNAThub
import os

from GNAThub import Log

from _sonarqube import SonarQube


class SonarRunner(GNAThub.Plugin):
    """GNATmetric plugin for GNAThub.
    """

    TOOL_NAME = 'Sonar Runner'

    def __init__(self):
        """Instance constructor."""

        super(SonarRunner, self).__init__()

    def setup(self):
        """Inherited."""

        # Do not call the super method: we do not need a database session to be
        # opened.

        SonarQube.make_workdir()

    def __cmd_line(self):
        """Returns command line for sonar runner execution."""

        return ['sonar-runner',
                '-Dproject.settings=%s' % SonarQube.configuration()]

    def display_command_line(self):
        """Inherited."""

        cmdline = ['-Dproject.settings=%s' %
                   os.path.relpath(SonarQube.configuration())]

        return ' '.join(cmdline)

    def execute(self):
        """Executes the Sonar Runner.

        SonarRunner.postprocess() will be called upon process completion.
        """

        Log.info('%s.run %s' % (self.fqn, self.display_command_line()))
        proc = GNAThub.Run(self.name, self.__cmd_line())
        self.postprocess(proc.status)

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output XML report on
        success.

        Sets the exec_status property according to the successful of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code != 0:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: execution failed' % self.fqn)
            return

        self.exec_status = GNAThub.EXEC_SUCCESS
