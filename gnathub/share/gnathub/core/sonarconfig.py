##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                     Copyright (C) 2013-2014, AdaCore                     ##
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

"""GNAThub plug-in for the generation of SonarQube Runner configuration file.

It exports the SonarConfig Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

# pylint: disable=import-error
from _sonarqube import SonarQube, SonarRunnerProperties


class SonarConfig(GNAThub.Plugin):
    """SonarConfig plugin for GNAThub.
    """

    name = 'sonar-config'

    def setup(self):
        """Inherited."""

        super(SonarConfig, self).setup()
        SonarQube.make_workdir()

    def execute(self):
        """Generates SonarQube Runner configuration file and dumps it."""

        self.info('generate %s' % SonarQube.CONFIGURATION)

        try:
            SonarRunnerProperties(self.log).write(SonarQube.configuration())

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate SonarRunner configuration')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
