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
import GNAThub.project

import os
import re

from GNAThub import GPSTarget, Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage
from GNAThub.utils import OutputParser


class GNATcheckOutputParser(OutputParser):
    """Defines custom output parser for the GNATcheck tool."""

    def on_stdout(self, text):
        with open(GNATcheck.logs(), 'w+a') as log:
            log.write(text)

    def on_stderr(self, text):
        with open(GNATcheck.logs(), 'w+a') as log:
            log.write(text)


class GNATcheck(GNAThub.Plugin):
    """GNATcheck plugin for GNAThub.

    Configures and executes GNATcheck, then analizes the output.
    """

    TOOL_NAME = 'GNATcheck'
    REPORT_FILE_NAME = 'gnatcheck.out'
    SLOC_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+'

    def __init__(self, session):
        """Instance constructor."""

        super(GNATcheck, self).__init__()

        self.session = session
        self.tool = dao.save_tool(self.session, self.name)

        # Create GPSTarget for GNATcheck execution
        self.process = GPSTarget(name=self.name,
                                 output_parser='gnatcheckoutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Creates GNAT Check command line argument list for GPS target."""

        prj_file = '-P%s' % GPSTarget.PRJ_FILE
        out_file = '-o=%s' % (os.path.join(GPSTarget.OBJ_DIR,
                                           self.REPORT_FILE_NAME))

        return [GPSTarget.GNAT, 'check', '--show-rule', '-s', out_file,
                prj_file]

    def __add_message(self, src, line, col_begin, rule_id, msg):
        """Add GNATcheck message to current session DB.

        Parameters:
            :param src: message source file
            :type src: a string
            :param line: message line number
            :type line: a string
            :param col_begin: message column number
            :type col_begin: a string
            :param rule_id: message's rule identifier
            :type rule_id: a string
            :param msg: description of the message
            :type msg: a string
        """

        rule = dao.get_or_create_rule(self.session, self.tool,
                                      db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line)

        if not line:
            Log.warn('Skipping message: %s, file not found: %s' % (msg, src))
            return

        line_message = LineMessage(col_begin=col_begin)
        line_message.message = Message(msg, rule)
        line.messages.append(line_message)

    def __parse_line(self, line):
        """Parses a GNATcheck message line and add the message to the current
        database session.

        Retrieves following informations:
            - source basename,
            - line in source,
            - rule identification,
            - message description

        PARAMETERS
            :param line: The text line to parse
            :type line: a string
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
        """Parses only one line and extracts the rule ID and message, then adds
        it to the database.

        PARAMETERS
            :param line: The text line to parse
            :type line: a string
        """

        # Look for the source file that instanciates the generic (last in the
        # list of "instance at...")
        match = re.search(self.SLOC_PATTERN, line)

        # Split to have a list with 'commands-generic_asynchronous.ads:57:15
        # instance at...' and 'message + rule id'
        msg_split = re.split(self.SLOC_PATTERN, line)

        try:
            # Retreive info on the main file
            # start_error = ['vsearch.adb', '231', '4', '']
            start_error = match.group(0).split(':')

            # Parsing message location information
            src = start_error[0].strip()
            line = start_error[1]
            col_begin = start_error[2]

            # Parsing message's rule information
            ruleid_msg = msg_split[1].split('[', 1)
            rule_id = ruleid_msg[1].strip()[:-1]

            msg = '%s (%s %s)' % (ruleid_msg[0].strip(),
                                  msg_split[0].strip(),
                                  src)

            # Create orm object for the mesage and add it to the session
            self.__add_message(src, line, col_begin, rule_id, msg)

        except IndexError:
            Log.warn('Unexpected error for message at: %s:%s' % (src, line))

    def parse_output_file(self):
        """Parses GNATcheck output file report

        Identify 2 type of mmessages with different format:
            - basic meesage message
            - message for packag instanciation

        RETURNS
            EXEC_SUCCES if changes has been committed to the DB, or EXEC_FAIL
            if an error occured when reading the output file
            :rtype: a number
        """

        # Regex to identify line that contains message
        ERROR_PATTERN = '%s:\s.+[[].*[]]\s*' % self.SLOC_PATTERN
        INSTANCE_PATTERN = '%s instance at %s.+' % (self.SLOC_PATTERN,
                                                    self.SLOC_PATTERN)

        prog_error = re.compile(ERROR_PATTERN)
        prog_instance = re.compile(INSTANCE_PATTERN)

        report = os.path.join(GNAThub.project.object_dir(),
                              self.REPORT_FILE_NAME)

        try:
            with open(report, 'r') as output:
                for line in output.readlines():
                    # Parse basic message line
                    if prog_error.match(line):
                        self.__parse_line(line)

                    # Parse message line for package instanciation
                    if prog_instance.match(line):
                        self.__parse_instance_line(line)

            # Commit object added and modified to the session
            self.session.commit()
            return GNAThub.EXEC_SUCCESS

        except IOError as e:
            Log.warn(str(e))
            return GNAThub.EXEC_FAIL

    def execute(self):
        """Inherited."""

        status = self.process.execute()

        if status == GNAThub.EXEC_FAIL:
            Log.warn('%s returned on failure' % self.name)
            Log.warn('See log file: %s' % GNATcheck.logs())

        return self.parse_output_file()
