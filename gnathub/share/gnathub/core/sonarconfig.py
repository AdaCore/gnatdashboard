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

"""GNAThub plug-in for the generation of SonarQube Runner configuration file.

It exports the SonarConfig Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import ConfigParser

import GNAThub.project

import os

from GNAThub import Log

# pylint: disable=F0401
# Disable: Unable to import '{}'
from _sonarqube import SonarQube


class _SonarConfiguration(object):
    """Represent Sonar configuration"""

    SONAR_SECTION = 'Sonar'

    CONFIG = {'sonar.language': ('ada', None),
              'sonar.sourceEncoding': ('UTF-8', 'Source_Encoding'),
              'sonar.sources': ('.', None),
              'sonar.projectVersion': ('1.0-SNAPSHOT', 'Project_Version'),
              'sonar.projectName': (GNAThub.project.name(), 'Project_Name'),
              'sonar.projectKey': ('%s::Project' % GNAThub.project.name(),
                                   'Project_Key'),
              'sonar.ada.qmt.db.path': (GNAThub.database(), None)}

    def _add(self, config, key, value, attribute=None):
        """Adds property in sonar configuration

        PARAMETERS
            :param config: the configuration object.
            :type config: a ConfigParser.ConfigParser object.
            :param key: property key.
            :type key: a string.
            :param value: property value.
            :type value: a string.
            :param attribute: custom project file attribute for this key.
                If None, the default value will be used. Defaults to None.
            :type value: a string.
        """

        section = _SonarConfiguration.SONAR_SECTION

        if attribute:
            attr_value = GNAThub.project.property_as_string(attribute)
            if attr_value:
                Log.debug('%s.%s: overriding default with %s' %
                          (section, key, attr_value))
                value = attr_value

        Log.debug('%s.%s = %s' % (section, key, value))
        config.set(section, key, value)

    def write(self, filename):
        """Dumps sonar-project.properties file in sonar working directory.

        PARAMETERS
            :param filename: the configuration file name.
            :type filename: a string.
        """

        config = ConfigParser.ConfigParser()

        # Enable case-sensitive keys feature
        config.optionxform = str

        # Create the [Sonar] section in the configuration file
        config.add_section(_SonarConfiguration.SONAR_SECTION)

        # Set properties
        for key, value in _SonarConfiguration.CONFIG.iteritems():
            # Unpack the tuple containing the default value and the custom
            # project attribute for this key.
            default, attribute = value

            # Insert the key in the configuration file
            self._add(config, key, default, attribute)

        with open(filename, 'w') as configuration:
            config.write(configuration)


class SonarConfig(GNAThub.Plugin):
    """SonarConfig plugin for GNAThub.
    """

    TOOL_NAME = 'Sonar Config'

    def setup(self):
        """Inherited."""

        # Do not call the super method: we do not need a database session to be
        # opened.

        SonarQube.make_workdir()

    def display_command_line(self):
        """Inherited."""

        cmdline = super(SonarConfig, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(SonarQube.configuration())])

        return cmdline

    def execute(self):
        """Generates SonarQube Runner configuration file and dumps it."""

        try:
            config = _SonarConfiguration()
            config.write(SonarQube.configuration())

            self.exec_status = GNAThub.EXEC_SUCCESS

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.fatal(str(ex))

        # Ensure that we don't break the plugin chain.
        self.ensure_chain_reaction(async=True)
