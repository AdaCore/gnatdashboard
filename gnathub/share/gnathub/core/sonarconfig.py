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

import ConfigParser

import GNAThub

# pylint: disable=F0401
# Disable: Unable to import '{}'
from _sonarqube import SonarQube


class _SonarConfiguration(object):
    """Represent Sonar configuration"""

    SONAR_SECTION = 'Sonar'

    SONAR_ATTRIBUTES = {
        'language': ('ada', None),
        'sourceEncoding': ('UTF-8', 'Source_Encoding'),
        'sources': ('.', None),
        'projectVersion': ('1.0-SNAPSHOT', 'Project_Version'),
        'projectName': (GNAThub.Project.name(), 'Project_Name'),
        'projectKey': ('%s::Project' % GNAThub.Project.name(), 'Project_Key'),
        'ada.gnathub.db': (GNAThub.database().replace('\\', '\\\\'), None)}

    def __init__(self, logger=None):
        self.log = logger

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

        if attribute and GNAThub.Project.property_as_string(attribute):
            value = GNAThub.Project.property_as_string(attribute)
            self.log.debug('%s = %s (overriding default)', key, value)
        else:
            self.log.debug('%s = %s', key, value)

        config.set(_SonarConfiguration.SONAR_SECTION, key, value)

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
        for key, value in _SonarConfiguration.SONAR_ATTRIBUTES.iteritems():
            # Unpack the tuple containing the default value and the custom
            # project attribute for this key.
            default, attribute = value

            # Insert the key in the configuration file
            self._add(config, 'sonar.%s' % key, default, attribute)

        with open(filename, 'w') as configuration:
            config.write(configuration)


class SonarConfig(GNAThub.Plugin):
    """SonarConfig plugin for GNAThub.
    """

    name = 'sonar-config'

    def setup(self):
        """Inherited."""

        # Do not call the super method: we do not need a database session to be
        # opened.

        SonarQube.make_workdir()

    def execute(self):
        """Generates SonarQube Runner configuration file and dumps it."""

        self.info('generate %s' % SonarQube.CONFIGURATION)

        try:
            config = _SonarConfiguration(logger=self.log)
            config.write(SonarQube.configuration())

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate SonarRunner configuration')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
