import GPS
import os
import qmt_api.utils
import logging
import ConfigParser
from qmt_api import utils
from sonarconfig import SonarConfiguration
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


## Sonarrunner ################################################################
##
class Sonarrunner(Plugin):
    LOG_FILE_NAME='sonar-runner.log'
    DIR='sonar'

    def __init__ (self, session):
        super(Sonarrunner, self).__init__('Sonar Runner')
        self.working_dir = os.path.join(qmt_api.utils.get_qmt_root_dir(), self.DIR)
        self.process = GPSTarget(name=self.name,
                                 output_parser='sonarrunneroutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Return command line for sonar runner execution """
        # Set specific location for Sonar project properties file
        sonar_conf = '-Dproject.settings=%s' % SonarConfiguration.FILE_NAME
        return ['sonar-runner', sonar_conf]

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

