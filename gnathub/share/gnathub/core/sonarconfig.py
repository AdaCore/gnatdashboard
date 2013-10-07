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


class SonarConfiguration(object):
    """Represent Sonar configuration"""

    MAIN_SECTION = 'Sonar'
    FILE_NAME = 'sonar-project.properties'
    CONFIG = {'sonar.language': 'ada',
              'sonar.sourceEncoding': 'UTF-8',
              'sonar.sources': '.',
              'sonar.projectVersion': '1.0-SNAPSHOT'}

    def __init__(self, deposit_dir):
        """Initialise """
        # Set configuration file path
        self.config_file = os.path.join(deposit_dir, self.FILE_NAME)
        # Create a configuration object
        self.config = ConfigParser.ConfigParser()
        # Enable case sensitive for key
        self.config.optionxform = str
        self.config.add_section(self.MAIN_SECTION)

        # Set property value
        self.add('sonar.language', 'ada')
        self.add('sonar.sources', '.')

        self.add('sonar.sourceEncoding', 'UTF-8',
                 GNAThub.project.property_as_string('Source_Encoding'))

        self.add('sonar.projectVersion', '1.0-SNAPSHOT',
                 GNAThub.project.property_as_string('Project_Version'))

        self.add('sonar.projectName', GNAThub.project.name(),
                 GNAThub.project.property_as_string('Project_Name'))

        self.add('sonar.projectKey', '%s::Project' %
                 GNAThub.project.name(),
                 GNAThub.project.property_as_string('Project_Key'))

        self.add('sonar.ada.qmt.db.path', GNAThub.database())

    def add(self, key, value, custom_value=None):
        """Add property in sonar configuration

           Parameters:
            - key: property key
            - value: property value
            - custom_value: custom value retrieve from project file
        """
        if custom_value:
            value = custom_value
        self.config.set(self.MAIN_SECTION, key, value)

    def export(self):
        """Dump sonar-project.properties file in sonar working directory"""
        with open(self.config_file, 'wb') as sonar_file:
            self.config.write(sonar_file)


class SonarConfig(GNAThub.Plugin):
    DIR = 'sonar'
    TOOL_NAME = 'Sonar Configuration'

    def __init__(self, session):
        super(SonarConfig, self).__init__()

    def setup(self):
        self.working_dir = os.path.join(GNAThub.root(), self.DIR)
        self.sonar_conf = SonarConfiguration(self.working_dir)

    def execute(self):
        """Setup for sonar runner execution

            - Create sonar directory in project_object_dir/gnathub
            - Export sonar configuration file in this directory
        """

        try:
            if not os.path.exists(self.working_dir):
                os.makedirs(self.working_dir)

            self.sonar_conf.export()

            return GNAThub.EXEC_SUCCESS

        except IOError as e:
            Log.fatal(str(e))
            return GNAThub.EXEC_FAIL
