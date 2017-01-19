# GNAThub (GNATdashboard)
# Copyright (C) 2016-2017, AdaCore
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

"""GNAThub plug-in for the SonarQube Scanner command-line tool.

It exports the SonarScanner class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in runner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import platform

import GNAThub

from _sonarqube import SonarQube
from GNAThub import Plugin, Reporter


class SonarScanner(Plugin, Reporter):
    """SonarQube Scanner plugin for GNAThub."""

    def __init__(self):
        super(SonarScanner, self).__init__()

    @property
    def name(self):
        return 'sonar-scanner'

    def setup(self):
        # Do not call the super method: we do not need a database session to be
        # opened.
        SonarQube.make_workdir()

    @staticmethod
    def __cmd_line():
        """Return command line for sonar scanner execution.

        :return: the SonarQube Scanner command line
        :rtype: collections.Iterable[str]
        """
        # Enable verbose and debugging output with -e and -X. This is handy for
        # debugging in case of issue in the SonarScanner step.
        cmdline = [
            'sonar-scanner', '-e', '-X',
            '-Dproject.settings={}'.format(SonarQube.configuration())
        ]
        if platform.system() == 'Windows':
            return ['cmd', '/c', ' '.join(cmdline)]
        return cmdline

    def report(self):
        """Execute the SonarQube Scanner.

        Returns according to the successful of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error
        """
        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status == 0 else GNAThub.EXEC_FAILURE
