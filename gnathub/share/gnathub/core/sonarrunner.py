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

import GNAThub
import os

from GNAThub import Log

from _sonarqube import SonarQube


class _SonarRunnerProtocol(GNAThub.LoggerProcessProtocol):
    def __init__(self, sonarrunner):
        GNAThub.LoggerProcessProtocol.__init__(self, sonarrunner)

    def processEnded(self, reason):
        GNAThub.LoggerProcessProtocol.processEnded(self, reason)

        self.plugin._postprocess(self.exit_code)

        # Ensure that we don't break the plugin chain.
        self.plugin.ensure_chain_reaction()


class SonarRunner(GNAThub.Plugin):
    """GNATmetric plugin for GNAThub.
    """

    TOOL_NAME = 'Sonar Runner'

    def __init__(self):
        super(SonarRunner, self).__init__()

        self.process = GNAThub.Process(self.name, self.__cmd_line(),
                                       _SonarRunnerProtocol(self))

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

        cmdline = super(SonarRunner, self).display_command_line()
        cmdline.append('-Dproject.settings=%s' %
                       os.path.relpath(SonarQube.configuration()))

        return cmdline

    def execute(self):
        """Executes the Sonar Runner.

        SonarRunner._postprocess() will be called upon process completion.
        """

        self.process.execute()

    def _postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output XML report on
        success.

        Sets the exec_status property according to the successfulness of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code != 0:
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            self.exec_status = GNAThub.EXEC_FAIL
            return

        self.exec_status = GNAThub.EXEC_SUCCESS
