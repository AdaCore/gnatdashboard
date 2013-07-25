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

## GnatmetricOutputParser #####################################################
##
class GnatcheckOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Gnatcheck.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Gnatcheck.get_log_file_path(), 'w+a') as log:
            log.write(text)

## Gnatcheck ##################################################################
##
class Gnatcheck(Plugin):
    """GNATcheck plugin for qualimetrics

       Launch GNATcheck
    """
    LOG_FILE_NAME='gnatcheck.log'
    REPORT_FILE_NAME='gnatcheck.out'

    def __init__ (self, session):
        super(Gnatcheck, self).__init__('GNATcheck')

        self.session = session
        self.tool = dao.save_tool(self.session, self.name)
        # Create GPSTarget for GNATmetric execution
        self.process = GPSTarget(name=self.name,
                                 output_parser='gnatcheckoutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Create Gnat Metric command line argument list for GPS target"""
        prj_file = '-P%s' % GPSTarget.PRJ_FILE
        out_file = '-o=%s/%s' % (GPSTarget.OBJ_DIR, self.REPORT_FILE_NAME)

        return [GPSTarget.GNAT, 'check', '--show-rule','-s', out_file, prj_file]

    def __add_message(self, src, line, col_begin, rule_id, msg):
        rule = dao.get_or_create_rule(self.session, self.tool, db.RULE_KIND,
                                      rule_id)
        line = dao.get_or_create_line(self.session, src, line)

        if line:
            line_message = LineMessage(col_begin=col_begin)
            line_message.message = Message(msg, rule)

            line.messages.append(line_message)
        else:
            logger.warn('Skipping message: %, file not found: %s' % (msg, src))

    def __parse_line(self, line):
        """Parse a GnatCheck  line

           Retrieves following informations: source basename, line in
           source, rule identification, violation message removing rubbish
           characters and save those informations as a violation
        """
        # Example with line : "input.adb:3:19: use clause for package
        # [USE_PACKAGE_Clauses]"

        # Set maxsplit at 3 because the rule id can contain ':'
        # split_1 = ['input.adb', '3', '19', ' use clause for package
        # [USE_PACKAGE_Clauses]']
        split_1 = line.split(':', 3)
        src = split_1[0]
        line = split_1[1]
        col_begin = split_1[2]

        # Remove rubbish characters
        # split_2 = [' use clause for package ', 'USE_PACKAGE_Clauses]']
        split_2 = split_1[-1].split('[', 1)
        # Remove the closing brace.
        rule_id = split_2[1].strip()[:-1]
        msg = split_2[0].strip()
        self.__add_message(src, line, col_begin, rule_id, msg)

    def __parse_instance_line(self, line):
        MAIN_SRC_FILE = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:'
        # Look for the source file that instanciates the generic (last un the
        # list of "instance at...")
        match = re.search(MAIN_SRC_FILE, line)
        # Split to have a list with 'commands-generic_asynchronous.ads:57:15
        # instance at...' and 'message + rule id'
        msg_split = re.split(MAIN_SRC_FILE, line)

        try:
            # Retreive info on the main file
            # start_error = ['vsearch.adb', '231', '4', '']
            start_error = match.group(0).split(':')
            src = start_error[0].strip()
            line = start_error[1]
            col_begin = start_erro[2]
            ruleid_msg = msg_split[1].split('[', 1)
            rule_id = ruleid_msg[1].strip()[:-1]
            msg = ruleid_msg[0].strip() + ' (' + msg_split[0].strip() + ' ' + src + ')'

            self.__add_message(src, line, col_begin, rule_id, msg)

        except IndexError:
            logger.warn('Unable to retrieve iformation from message at: %s:%s' % (src, line))

    def parse_metrix_xml_file(self):
        ERROR_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:\s.+[[].*[]]\s*'
        INSTANCE_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+ instance at [a-zA-Z-_.0-9]+:[0-9]+:[0-9]+.+'
        prog_error = re.compile(ERROR_PATTERN)
        prog_instance = re.compile(INSTANCE_PATTERN)

        report = os.path.join(utils.get_project_obj_dir(),
                              self.REPORT_FILE_NAME)
        try:
            with open(report, 'r') as output:

                for line in output.readlines():

                    if prog_error.match(line):
                        self.__parse_line(line)

                    if prog_instance.match(line):
                        self.__parse_instance_line(line)

            self.session.commit()
            return plugin.EXEC_SUCCESS
        except IOError as e:
            logger.warn(str(e))
            return plugin.EXEC_FAIL

    def execute(self):
        status = self.process.execute()
        if status == plugin.EXEC_FAIL:
            logging.warn('GNAT Metric execution returned on failure')
            logging.warn('For more details, see log file: %s' % self.get_log_file_path())
        return self.parse_metrix_xml_file()

#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
#print (output)

