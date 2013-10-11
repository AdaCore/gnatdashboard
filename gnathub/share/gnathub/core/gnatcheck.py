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

"""GNAThub plug-in for the GNATcheck command-line tool.

It exports the GNATcheck Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import GNAThub
import GNAThub.project

import os
import re

from _gnat import GNATToolProgressProtocol, SLOC_PATTERN, SLOC_NO_TAG_PATTERN

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage


class GNATcheck(GNAThub.Plugin):
    """GNATcheck plugin for GNAThub.

    Configures and executes GNATcheck, then analizes the output.
    """

    TOOL_NAME = 'GNATcheck'
    REPORT = 'gnatcheck.out'

    # Regex to identify lines that contain messages
    ERROR_PATTERN = '%s:\s.+[[].*[]]\s*' % SLOC_PATTERN
    INSTANCE_PATTERN = '%s instance at %s.+' % (SLOC_PATTERN,
                                                SLOC_NO_TAG_PATTERN)

    # GNATcheck exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        """Instance constructor."""

        super(GNATcheck, self).__init__()

        self.report = os.path.join(GNAThub.project.object_dir(), self.REPORT)

        self.process = GNAThub.Process(self.name, self.__cmd_line(),
                                       GNATToolProgressProtocol(self))

    def display_command_line(self):
        """Inherited."""

        cmdline = super(GNATcheck, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(self.report)])

        return cmdline

    def __cmd_line(self):
        """Creates GNATcheck command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnat', 'check', '--show-rule', '-o', self.report,
                '-P', GNAThub.project.path()]

    def execute(self):
        """Executes the GNATcheck.

        GNATcheck.postprocess() will be called upon process completion.
        """

        self.process.execute()

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code not in GNATcheck.VALID_EXIT_CODES:
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            self.exec_status = GNAThub.EXEC_FAIL
            return

        self.__parse_report()

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

        # pylint: disable=E1103
        # Disable "Module {} has no member {}" error
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
        match = re.search(SLOC_PATTERN, line)

        # Split to have a list with 'commands-generic_asynchronous.ads:57:15
        # instance at...' and 'message + rule id'
        msg_split = re.split(SLOC_PATTERN, line)

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

    def __parse_report(self):
        """Parses GNATcheck output file report.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error

        Identify 2 type of messages with different format:
            - basic message
            - message for package instantiation
        """

        Log.info('gnathub analyse %s' % os.path.relpath(self.report))

        Log.debug('%s: storing tool in database' % self.name)
        self.tool = dao.save_tool(self.session, self.name)

        prog_error = re.compile(self.ERROR_PATTERN)
        prog_instance = re.compile(self.INSTANCE_PATTERN)

        Log.debug('%s: parsing report: %s' % (self.name, self.report))

        if not os.path.exists(self.report):
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: no report found' % self.name)
            Log.error('%s: aborting analysis' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            return

        try:
            with open(self.report, 'r') as output:
                lines = output.readlines()
                total = len(lines)

                for index, line in enumerate(lines, start=1):
                    # Parse basic message line
                    if prog_error.match(line):
                        self.__parse_line(line)

                    # Parse message line for package instanciation
                    if prog_instance.match(line):
                        self.__parse_instance_line(line)

                    Log.progress(index, total, new_line=(index == total))

            self.session.commit()

            self.exec_status = GNAThub.EXEC_SUCCESS
            Log.debug('%s: all objects commited to database' % self.name)

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: unable to parse report' % self.name)
            Log.error(str(ex))
