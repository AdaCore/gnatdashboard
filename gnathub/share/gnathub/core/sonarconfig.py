############################################################################
#                                                                          #
#                               G N A T h u b                              #
#                                                                          #
#                     Copyright (C) 2013-2014, AdaCore                     #
#                                                                          #
# This is free software;  you can redistribute it  and/or modify it  under #
# terms of the  GNU General Public License as published  by the Free Soft- #
# ware  Foundation;  either version 3,  or (at your option) any later ver- #
# sion.  This software is distributed in the hope  that it will be useful, #
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License for  more details.  You should have  received  a copy of the GNU #
# General  Public  License  distributed  with  this  software;   see  file #
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy #
# of the license.                                                          #
#                                                                          #
############################################################################

"""GNAThub plug-in for the generation of SonarQube Runner configuration file

It exports the SonarConfig class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

from _sonarqube import SonarQube, SonarRunnerProperties


class SonarConfig(GNAThub.Plugin):
    """SonarConfig plugin for GNAThub"""

    def setup(self):
        super(SonarConfig, self).setup()
        SonarQube.make_workdir()

    @property
    def name(self):
        return 'sonar-config'

    def execute(self):
        """Generates SonarQube Runner configuration file and dumps it"""

        self.info('generate %s' % SonarQube.CONFIGURATION)

        try:
            SonarRunnerProperties(self.log).write(SonarQube.configuration())

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate SonarRunner configuration')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
