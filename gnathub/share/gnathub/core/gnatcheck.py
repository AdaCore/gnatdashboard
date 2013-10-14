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

# pylint: disable=F0401
# Disable: Unable to import '{}'
from _gnat import GNATToolProgressProtocol, SLOC_PATTERN

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
    _RULE_PATTERN = '(?P<message>.+)\s\[(?P<rule>[A-Za-z_]+)\]$'

    # Regular expression to match GNATcheck output and extract all relevant
    # information stored in it.
    _MESSAGE = re.compile('%s:\s%s' % (SLOC_PATTERN, _RULE_PATTERN))

    # GNATcheck exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        """Instance constructor."""

        super(GNATcheck, self).__init__()

        self.report = os.path.join(GNAThub.project.object_dir(), self.REPORT)

        self.gnatcheck = GNAThub.Process(self.name, self.__cmd_line(),
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

        return ['gnatcheck', '--show-rule', '-o', self.report,
                '-P', GNAThub.project.path()]

    def execute(self):
        """Executes the GNATcheck.

        GNATcheck.postprocess() will be called upon process completion.
        """

        self.gnatcheck.execute()

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code not in GNATcheck.VALID_EXIT_CODES:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            return

        self.__parse_report()

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
                    Log.debug('Parsing line: %s' % line)

                    match = self._MESSAGE.match(line)

                    if match:
                        Log.debug('Matched groups: %s' % str(match.groups()))
                        self.__parse_line(match)

                    Log.progress(index, total, new_line=(index == total))

            self.session.commit()

            self.exec_status = GNAThub.EXEC_SUCCESS
            Log.debug('%s: all objects commited to database' % self.name)

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: unable to parse report' % self.name)
            Log.error(str(ex))

    def __parse_line(self, regex):
        """Parses a GNATcheck message line and add the message to the current
        database session.

        Retrieves following informations:
            - source basename,
            - line in source,
            - rule identification,
            - message description

        PARAMETERS
            :param regex: the result of the MSG_RE regex.
            :type regex: a regex result.
        """

        # The following Regex results are explained using this example.
        # 'input.adb:3:19: use clause for package [USE_PACKAGE_Clauses]'

        # Extract each component from the message:
        #       ('input.adb', '3', '19', 'use clause for package',
        #        'USE_PACKAGE_Clauses')
        src = regex.group('file')
        line = regex.group('line')
        column = regex.group('column')
        message = regex.group('message')
        rule = regex.group('rule')

        self.__add_message(src, line, column, rule, message)

    def __add_message(self, src, line, col_begin, rule_id, msg):
        """Adds GNATcheck message to current session database.

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
