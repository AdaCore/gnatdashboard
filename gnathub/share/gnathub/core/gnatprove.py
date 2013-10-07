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

import GNAThub

import re

from GNAThub import GPSTarget, Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage
from GNAThub.utils import OutputParser


class GNATproveOutputParser(OutputParser):
    """Define custom output parser"""

    def on_stdout(self, text):
        with open(GNATprove.logs(), 'w+a') as log:
            log.write(text)

    def on_stderr(self, text):
        with open(GNATprove.logs(), 'w+a') as log:
            log.write(text)


class GNATprove(GNAThub.Plugin):
    """GNATprove plugin for GNAThub.

    Launch GNATprove
    """

    TOOL_NAME = 'GNATprove'
    SEVERITIES = {'info': 'INFO',
                  'warning': 'MINOR',
                  'error': 'MAJOR'}

    def __init__(self, session):
        super(GNATprove, self).__init__()

        self.session = session
        self.tool = dao.save_tool(self.session, self.name)

        # Create GPSTarget for GNATmetric execution
        parser = GNATproveOutputParser.__class__.__name__
        self.process = GPSTarget(name=self.name, output_parser=parser,
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Create Gnatprove  command line argument list for GPS target"""

        prj_file = '-P%s' % GPSTarget.PRJ_FILE

        return ['gnatprove', prj_file, '--show-tag']

    def __add_message(self, src, line, col_begin, rule_id, msg):
        rule = dao.get_or_create_rule(self.session, self.tool,
                                      db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line)

        if line:
            line_message = LineMessage(col_begin=col_begin)
            line_message.message = Message(msg, rule)

            line.messages.append(line_message)
        else:
            Log.warn('Skipping message: %s, file not found: %s' % (msg, src))

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
            severity = (self.SEVERITIES['error']
                        if len(split_1) == 4
                        else self.SEVERITIES[split_1[3].strip()])

            #split_2 = [' overflow check proved ', 'overflow_check]\n']
            split_2 = split_1[-1].split('[', 1)

            # Remove the closing brace.
            rule_id = '%s__%s' % (severity, split_2[1].strip()[:-1])
            msg = split_2[0].strip()

            self.__add_message(src, line, col_begin, rule_id, msg)

        except IndexError:
            Log.warn('Unexpected message format: %s:%s' % (src, line))

        except KeyError:
            Log.warn('Unknown category for: %s' % split_1[3].strip())

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
        SLOC_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+'
        MSG_PATTERN = '%s:(\s[a-z]:)?\s.+[[].*[]]\s*' % SLOC_PATTERN
        prog_msg = re.compile(MSG_PATTERN)

        try:
            with open(GNATprove.logs(), 'r') as output:
                for line in output.readlines():
                    if prog_msg.match(line):
                        self.__parse_line(line)

            # Commit object added and modified to the session then return
            # SUCCESS
            self.session.commit()
            return GNAThub.EXEC_SUCCESS

        except IOError as e:
            Log.warn(str(e))
            return GNAThub.EXEC_FAIL

    def execute(self):
        status = self.process.execute()

        if status == GNAThub.EXEC_FAIL:
            Log.warn('GNATprove returned on failure')
            Log.warn('see log file: %s' % self.get_log_file_path())

        return self.parse_output_file()
