import os
import re
import GPS
import logging
import qmt_api
from qmt_api import plugin
from qmt_api import utils
from qmt_api import Session, dao, db
from qmt_api.db import Rule, Message, LineMessage
from qmt_api.utils import OutputParser, create_parser
from qmt_api.plugin import GPSTarget, Plugin

logger = logging.getLogger(__name__)

## GnatproveOutputParser #####################################################
##
class GnatproveOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Gnatprove.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Gnatprove.get_log_file_path(), 'w+a') as log:
            log.write(text)

## Gnatprove ##################################################################
##
class Gnatprove(Plugin):
    """GNATprove plugin for qualimetrics

       Launch GNATprove
    """
    LOG_FILE_NAME='gnatprove.log'
    SEVERITIES={'info'    : 'INFO',
                'warning' : 'MINOR',
                'error'   : 'MAJOR'}

    def __init__ (self, session):
        super(Gnatprove, self).__init__('GNATprove')

        self.session = session
        self.tool = dao.save_tool(self.session, self.name)
        # Create GPSTarget for GNATmetric execution
        self.process = GPSTarget(name=self.name,
                                 output_parser='gnatproveoutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Create Gnatprove  command line argument list for GPS target"""
        prj_file = '-P%s' % GPSTarget.PRJ_FILE

        return ['gnatprove', prj_file, '--show-tag']

    def __add_message(self, src, line, col_begin, rule_id, msg):
        rule = dao.get_or_create_rule (self.session, self.tool,
                                       db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line)

        if line:
            line_message = LineMessage(col_begin=col_begin)
            line_message.message = Message(msg, rule)

            line.messages.append(line_message)
        else:
            logger.warn('Skipping message: %s, file not found: %s' % (msg, src))

    def __parse_line(self, line):
        """Parse a GnatCheck message line and add the message to the current DB
            session.

           Parameter:
            - line: text line to parse

           Retrieves following informations:
            - source basename,
            - line in source,
            - rule identification,
            - message description
        """
        try:
            # Example with line: "nose_gear.adb:144:50: info: overflow check
            # proved [overflow_check]"

            # split_1 = ['nose_gear.adb', '140', '44', ' info', ' overflow
            # check proved [overflow_check]\n']
            split_1 = line.split(':')

            src = split_1[0]
            line = split_1[1]
            col_begin = split_1[2]
            severity = (self.SEVERITIES['error'] if len(split_1) == 4
                                      else self.SEVERITIES[split_1[3].strip()])

            #split_2 = [' overflow check proved ', 'overflow_check]\n']
            split_2 = split_1[-1].split('[', 1)

            # Remove the closing brace.
            rule_id = '%s__%s' % (severity, split_2[1].strip()[:-1])
            msg = split_2[0].strip()

            self.__add_message(src, line, col_begin, rule_id, msg)

        except IndexError:
            logger.warn('Unable to retrieve iformation from message at: %s:%s' % (src, line))
        except KeyError:
            logger.warn('Unknow category for: %s' % split_1[3].strip())

    def parse_output_file(self):
        """Parse GNATprove output file report

           Identify 2 type of mmessages with different format:
            - basic meesage message
            - message for packag instanciation

          Return:
            - EXEC_SUCCES: if changes has been committed to the DB
            - EXEC_FAIL: if an error occured when reading the output file
        """
        # Initialise regex to identify line that contains message
        MSG_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:(\s[a-z]:)?\s.+[[].*[]]\s*'
        prog_msg = re.compile(MSG_PATTERN)

        try:
            with open(Gnatprove.get_log_file_path(), 'r') as output:

                for line in output.readlines():

                    if prog_msg.match(line):
                        self.__parse_line(line)


            # Commit object added and modified to the session then return
            # SUCCESS
            self.session.commit()
            return plugin.EXEC_SUCCESS

        except IOError as e:
            logger.warn(str(e))
            return plugin.EXEC_FAIL

    def execute(self):
        status = self.process.execute()
        if status == plugin.EXEC_FAIL:
            logging.warn('GNATprove returned on failure')
            logging.warn('see log file: %s' % self.get_log_file_path())
        return self.parse_output_file()

