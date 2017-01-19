# GNAThub (GNATdashboard)
# Copyright (C) 2013-2017, AdaCore
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

"""GNAThub plug-in for the generation of SonarQube Scanner configuration file.

It exports the SonarConfig class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

from _sonarqube import SonarQube, SonarScannerProperties
from GNAThub import Plugin, Runner


class SonarConfig(Plugin, Runner):
    """SonarConfig plugin for GNAThub."""

    def setup(self):
        super(SonarConfig, self).setup()
        SonarQube.make_workdir()

    @property
    def name(self):
        return 'sonar-config'

    def run(self):
        """Generate SonarQube Scanner configuration file."""

        self.info('generate %s' % SonarQube.CONFIGURATION)
        try:
            SonarScannerProperties(self.log).write(SonarQube.configuration())
        except IOError as why:
            self.log.exception(
                'SonarQube Scanner configuration generation failed')
            self.error(str(why))
            return GNAThub.EXEC_FAILURE
        else:
            return GNAThub.EXEC_SUCCESS
