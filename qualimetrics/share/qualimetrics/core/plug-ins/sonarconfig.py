import GPS
import os
import qmt_api.utils
import logging
import ConfigParser
from qmt_api import utils
from xml.etree import ElementTree
from qmt_api.utils import OutputParser, create_parser
from qmt_api import plugin
from qmt_api.plugin import Plugin, GPSTarget
from qmt_api.db import Rule, Message
from qmt_api import Session
from qmt_api import dao

logger = logging.getLogger(__name__)

## SonarConfiguration #######################################################
##
class SonarConfiguration(object):
    """Represent Sonar configuration"""

    MAIN_SECTION = 'Sonar'
    FILE_NAME = 'sonar-project.properties'
    CONFIG = {'sonar.language'       : 'ada',
              'sonar.sourceEncoding' : 'UTF-8',
              'sonar.sources'        : '.',
              'sonar.projectVersion' : '1.0-SNAPSHOT'}

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
                 utils.get_qmt_property_str('Source_Encoding'))

        self.add('sonar.projectVersion', '1.0-SNAPSHOT',
                 utils.get_qmt_property_str('Project_Version'))

        self.add('sonar.projectName',
                  qmt_api.utils.get_project_name(),
                  utils.get_qmt_property_str('Project_Name'))

        self.add('sonar.projectKey', '%s::project' %
                 qmt_api.utils.get_project_name(),
                 utils.get_qmt_property_str('Project_Key'))

        self.add('sonar.ada.qmt.db.path', qmt_api.utils.get_db_path())

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

## Sonarconfig ################################################################
##
class Sonarconfig(Plugin):
    DIR='sonar'

    def __init__ (self, session):
        super(Sonarconfig, self).__init__('Sonar Configuration')

    def setup(self):
        self.working_dir = os.path.join(qmt_api.utils.get_qmt_root_dir(), self.DIR)
        self.sonar_conf = SonarConfiguration(self.working_dir)

    def execute(self):
        """Setup for sonar runner execution

            - Create sonar directory in project_object_dir/qualimetric
            - Export sonar configuration file in this directory
        """
        try:
            if not os.path.exists(self.working_dir):
                os.makedirs(self.working_dir)
            self.sonar_conf.export()
            return plugin.EXEC_SUCCESS
        except IOError as e:
            logger.error(e)
            return plugin.EXEC_FAIL
