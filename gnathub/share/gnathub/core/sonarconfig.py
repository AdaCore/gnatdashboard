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

import ConfigParser

import GNAThub.utils
import GNAThub.project

import os

from GNAThub import Log


class _SonarConfiguration(object):
    """Represent Sonar configuration"""

    SONAR_SECTION = 'Sonar'
    FILE_NAME = 'sonar-project.properties'

    CONFIG = {'sonar.language': ('ada', None),
              'sonar.sourceEncoding': ('UTF-8', 'Source_Encoding'),
              'sonar.sources': ('.', None),
              'sonar.projectVersion': ('1.0-SNAPSHOT', 'Project_Version'),
              'sonar.projectName': (GNAThub.project.name(), 'Project_Name'),
              'sonar.projectKey': ('%s :: Project' % GNAThub.project.name(),
                                   'Project_Key'),
              'sonar.ada.qmt.db.path': (GNAThub.database(), None)}

    def __init__(self, deposit_dir):
        """Initialize."""

        # Compute the configuration file path
        self._config_file = os.path.join(deposit_dir,
                                         _SonarConfiguration.FILE_NAME)

        self.config = ConfigParser.ConfigParser()

        # Enable case-sensitive keys feature
        self.config.optionxform = str

        # Create the [Sonar] section in the configuration file
        self.config.add_section(_SonarConfiguration.SONAR_SECTION)

        # Set properties
        for key, value in _SonarConfiguration.CONFIG.iteritems():
            # Unpack the tuple containing the default value and the custom
            # project attribute for this key.
            default, attribute = value

            # Insert the key in the configuration file
            self._add(key, default, attribute)

    def path(self):
        """Returns the absolute path to the configuration file.

        RETURNS
            :rtype: a string
        """

        return self._config_file

    def _add(self, key, value, attribute=None):
        """Adds property in sonar configuration

           Parameters:
            - key: property key
            - value: property value
            - custom_value: custom value retrieve from project file
        """

        if attribute:
            attr_value = GNAThub.project.property_as_string(attribute)
            if attr_value:
                value = attr_value

        self.config.set(_SonarConfiguration.SONAR_SECTION, key, value)

    def write(self):
        """Dumps sonar-project.properties file in sonar working directory"""

        with open(self._config_file, 'w') as config_file:
            self.config.write(config_file)


class SonarConfig(GNAThub.Plugin):
    TOOL_NAME = 'Sonar Configuration'
    EXEC_DIRECTORY_NAME = 'sonar'

    def __init__(self):
        super(SonarConfig, self).__init__()

        self.workdir = os.path.join(GNAThub.root(), self.EXEC_DIRECTORY_NAME)

        if not os.path.exists(self.workdir):
            os.makedirs(self.workdir)

        self._config = _SonarConfiguration(self.workdir)

    def setup(self):
        """Inherited."""

        # Do not call the super method: we do not need a database session to be
        # opened.
        pass

    def display_command_line(self):
        """Inherited."""

        cmdline = super(SonarConfig, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(self._config.path())])

        return cmdline

    def execute(self):
        """Setup for sonar runner execution

            - Create sonar directory in project_object_dir/gnathub
            - Export sonar configuration file in this directory
        """

        try:
            self._config.write()
            self.exec_status = GNAThub.EXEC_SUCCESS

        except IOError as e:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.fatal(str(e))

        # Ensure that we don't break the plugin chain.
        self.ensure_chain_reaction(async=True)
