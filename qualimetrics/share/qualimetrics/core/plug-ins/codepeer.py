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

## CodepeerOutputParser #######################################################
##
class MsgReaderOutputParser(OutputParser):
    """Define custom output parser for codepeer_msg_reader"""
    def on_stdout(self,text):
        with open (Codepeer.CSV_REPORT_PATH, 'w+a') as output:
            output.write(text)

## CodepeerOutputParser #######################################################
##
class CodepeerOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Codepeer.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Codepeer.get_log_file_path(), 'w+a') as log:
            log.write(text)

## Codepeer ##################################################################
##
class Codepeer(Plugin):
    """CodePeer plugin for qualimetrics

       Launch CodePeer
    """
    LOG_FILE_NAME='codepeer.log'

    def __init__ (self, session):
        super(Codepeer, self).__init__('CodePeer')

        self.session = session
        self.tool = dao.save_tool(self.session, self.name)
        # Create GPSTarget for GNATmetric execution
        self.codepeer = GPSTarget(name=self.name,
                                 output_parser='codepeeroutputparser',
                                 cmd_args=self.__codepeer_cmd_line())

        self.codepeer_msg_reader = GPSTarget(name='Codepeer Message Reader',
                                 output_parser='msgreaderoutputparser',
                                 cmd_args=self.__msg_reader_cmd_line())

        self.CSV_REPORT_PATH = os.path.join(
           utils.get_project_obj_dir(), 'codepeer.csv')

    def __codepeer_cmd_line(self):
        """Create codepeer command line argument list for GPS target
        """
        prj_file = '-P%s' % GPSTarget.PRJ_FILE
        return ['codepeer', prj_file, '-update-scil', '--subdirs=codepeer']

    def __msg_reader_cmd_line(self):
        """Create codepeer_msg_reader command line argument list for GPS target
        """
        msg_dir = 'codepeer/%s.output' % utils.get_project_name().lower()
        msg_dir_path =  '%s/%s' % (GPSTarget.OBJ_DIR, msg_dir)

        return ['codepeer_msg_reader', '-csv', msg_dir_path]

    def __add_message(self, src, line_num, col_begin, rule_id, msg, category):
        rule = dao.get_or_create_rule(self.session, self.tool,
                                      db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line_num)
        category = dao.get_or_create_category(self.session, category)

        if line:
            line_message = LineMessage(col_begin=col_begin)
            line_message.message = Message(msg, rule, category)

            line.messages.append(line_message)
        else:
            logger.warn('Skipping message: %s, file not found: %s' % (msg, src))

    def parse_csv_output(self):
        try:
            with open(self.CSV_REPORT_PATH, 'r') as cp_output:

                # First line does not contain message
                for line in cp_output.readlines()[1:]:
                    #Set maxsplit at 4 because the rule message can contain commas
                    line_splited = line.split(',', 8)

                    # Parsing source file information
                    source = line_splited[0]
                    line = line_splited[1]
                    col_begin = line_splited[2]

                    # Parsing rule's information
                    rule_key = line_splited[3]
                    severity = line_splited[6]
                    category = line_splited[7]
                    # Removes double quote
                    message = line_splited[8][1:-1]

                    # In Sonar Codepeer rule repository, a rule has been
                    # duplicated for each priorities and each category.
                    # See TN L919-022
                    category = '%s__%s' % (severity.upper(), category.upper())

                    # Creates and saves the violation in given report
                    self.__add_message(source, line, col_begin,
                                       rule_key, message, category)
            self.session.commit()
            return plugin.EXEC_SUCCESS
        except IOError as e:
            logger.warn(str(e))
            return plugin.EXEC_FAIL

    def execute(self):
        if not os.path.exists(self.CSV_REPORT_PATH):
            status = self.codepeer.execute()
            if status:
                status = self.codepeer_msg_reader.execute()
            if status == plugin.EXEC_FAIL:
                logging.warn('GNAT Metric execution returned on failure')
                logging.warn('For more details, see log file: %s' % self.get_log_file_path())
                return plugin.EXEC_FAIL
        return self.parse_csv_output()

#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
#print (output)

