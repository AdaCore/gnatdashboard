import GPS
import os
import qmt_api.utils
import ConfigParser
from xml.etree import ElementTree
from qmt_api.utils import OutputParser, create_parser
from qmt_api.plugin import Plugin
from qmt_api.db import Rule, Message
from qmt_api import Session
from qmt_api import dao

# Initialize the targets
xml_base = """<?xml version="1.0"?>
<GPS>
 <builder-mode name="default">
  <description></description>
 </builder-mode>

<target-model name="sonar-runner" category="">
   <description>Generic launch of the Sonar Runner</description>
</target-model>

<target model="sonar-runner" category="default" name="Sonar Runner">
    <command-line>
      <arg>sonar-runner</arg>
    </command-line>
    <output-parsers>sonarrunneroutputparser output_collector</output-parsers>
</target>

<target model="sonar-runner" category="debug" name="Sonar Runner in debug mode">
    <command-line>
      <arg>sonar-runner</arg>
      <arg>-X</arg>
    </command-line>
    <output-parsers>sonarrunneroutputparser output_collector</output-parsers>
</target>

</GPS>
"""

## GnatmetricOutputParser #####################################################
##
class SonarrunnerOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Sonarrunner.get_log_file_path('sonar-runner'), 'w+a') as log:
            log.write(text)

## SonarConfiguration #######################################################
##
class SonarConfiguration(object):
    MAIN_SECTION = 'Sonar'
    FILE_NAME = 'sonar-project.properties'
    DEFAULT_CONFIG = {'sonar.language'       : 'ada',
                      'sonar.sourceEncoding' : 'UTF-8',
                      'sonar.sources'        : '.',
                      'sonar.projectVersion' : '1.0-SNAPSHOT'}

    def __init__(self, deposit_dir):
        # Set configuration file path
        self.config_file = os.path.join(deposit_dir, self.FILE_NAME)
        # Create a configuration object
        self.config = ConfigParser.ConfigParser()
        # Enable case sensitive for key
        self.config.optionxform = str
        self.config.add_section(self.MAIN_SECTION)
        # Initialise configuration with default values
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
        self.config.set(self.MAIN_SECTION, key, value)

    def export(self):
        # Dump the configuration file
        with open(self.config_file, 'wb') as sonar_file:
            self.config.write(sonar_file)

## Gnatmetric ################################################################
##
class Sonarrunner(Plugin):
    DIR='sonar'

    def __init__ (self, session):
        super(Sonarrunner, self).__init__('Sonar Runner')
        self.working_dir = os.path.join(qmt_api.utils.get_qmt_root_dir(), self.DIR)
        self.sonar_conf = SonarConfiguration(self.working_dir)

    def setup(self):
       # Create working directory
       if not os.path.exists(self.working_dir):
           os.makedirs(self.working_dir)

       # Create sonar configuration file
       self.sonar_conf.export()

    def execute(self):
       # Build target
       GPS.parse_xml(xml_base)
       target = GPS.BuildTarget("Sonar Runner")
       target.execute(extra_args='-Dproject.settings=%s' % self.sonar_conf.config_file)

