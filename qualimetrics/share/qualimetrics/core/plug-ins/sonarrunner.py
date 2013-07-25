import GPS
import os
import qmt_api.utils
import logging
import ConfigParser
from xml.etree import ElementTree
from qmt_api.utils import OutputParser, create_parser
from qmt_api import plugin
from qmt_api.plugin import Plugin, GPSTarget
from qmt_api.db import Rule, Message
from qmt_api import Session
from qmt_api import dao

logger = logging.getLogger(__name__)

## GnatmetricOutputParser #####################################################
##
class SonarrunnerOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Sonarrunner.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Sonarrunner.get_log_file_path(), 'w+a') as log:
            log.write(text)

## SonarConfiguration #######################################################
##
class SonarConfiguration(object):
    """Represent Sonar configuration"""

    MAIN_SECTION = 'Sonar'
    FILE_NAME = 'sonar-project.properties'
    DEFAULT_CONFIG = {'sonar.language'       : 'ada',
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

        # Initialise configuration with project default values
        self.config.set(self.MAIN_SECTION, 'sonar.ada.qmt.db.path',
                        qmt_api.utils.get_db_path())
        self.config.set(self.MAIN_SECTION, 'sonar.projectName',
                        qmt_api.utils.get_project_name())
        self.config.set(self.MAIN_SECTION, 'sonar.projectKey',
                        '%s::project' % qmt_api.utils.get_project_name())

        for key in self.DEFAULT_CONFIG:
            # Will be surrounded with if, when project file attributes
            # will be retrievable
            self.config.set(self.MAIN_SECTION, key, self.DEFAULT_CONFIG[key])

    def add(self, key, value):
        """Add property in sonar configuration"""
        self.config.set(self.MAIN_SECTION, key, value)

    def export(self):
        """Dump sonar-project.properties file in sonar working directory"""
        # Dump the configuration file
        with open(self.config_file, 'wb') as sonar_file:
            self.config.write(sonar_file)

## Sonarrunner ################################################################
##
class Sonarrunner(Plugin):
    LOG_FILE_NAME='sonar-runner.log'
    DIR='sonar'

    def __init__ (self, session):
        super(Sonarrunner, self).__init__('Sonar Runner')
        self.working_dir = os.path.join(qmt_api.utils.get_qmt_root_dir(), self.DIR)
        self.sonar_conf = SonarConfiguration(self.working_dir)
        self.process = GPSTarget(name=self.name,
                                 output_parser='sonarrunneroutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Return command line for sonar runner execution """
        # Set specific location for Sonar project properties file
        sonar_conf = '-Dproject.settings=%s' % self.sonar_conf.config_file
        return ['sonar-runner', sonar_conf]

    def setup(self):
        """Setup for sonar runner execution

            - Create sonar directory in project_object_dir/qualimetric
            - Export sonar configuration file in this directory
        """
        if not os.path.exists(self.working_dir):
            os.makedirs(self.working_dir)
        self.sonar_conf.export()

    def execute(self):
        status = self.process.execute()

        # if sonar-runner execution has failed
        if status  == plugin.EXEC_FAIL:
            logger.warn('Sonar runner execution returned on failure')
            logger.warn('For more details, see log file: %s' % self.get_log_file_path())
        else:
            # If sonar-runner execution succeed or did not happened,
            # in last case GPSTarget manage error output so just
            # return status
            return status

