##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                     Copyright (C) 2013-2014, AdaCore                     ##
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
module and load it as part of the GNAThub default execution.
"""

import os
import re

# pylint: disable=F0401
# Disable: Unable to import '{}'
from _gnat import SLOC_PATTERN

import GNAThub
from GNAThub import System


class GNATcheck(GNAThub.Plugin):
    """GNATcheck plugin for GNAThub.

    Configures and executes GNATcheck, then analyzes the output.
    """

    name = 'gnatcheck'

    # Regex to identify lines that contain messages
    _RULE_PATTERN = '(?P<message>.+)\s\[(?P<rule>[A-Za-z_]+)\]$'

    # Regular expression to match GNATcheck output and extract all relevant
    # information stored in it.
    _MESSAGE = re.compile('%s:\s%s' % (SLOC_PATTERN, _RULE_PATTERN))

    # GNATcheck exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        super(GNATcheck, self).__init__()

        self.tool = None
        self.report = os.path.join(GNAThub.Project.object_dir(),
                                   '%s.out' % self.name)

    def __cmd_line(self):
        """Creates GNATcheck command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnatcheck', '--show-rule', '-o', self.report,
                '-P', GNAThub.Project.path(), '-j%d' % GNAThub.jobs()]

    def execute(self):
        """Executes the GNATcheck.

        GNATcheck.postprocess() will be called upon process completion.
        """

        proc = GNAThub.Run(self.name, self.__cmd_line())
        self.postprocess(proc.status)

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAILURE: on any error
        """

        if exit_code not in GNATcheck.VALID_EXIT_CODES:
            self.exec_status = GNAThub.EXEC_FAILURE
            return

        self.__parse_report()

    def __parse_report(self):
        """Parses GNATcheck output file report.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAILURE: on any error

        Identify 2 type of messages with different format:
            - basic message
            - message for package instantiation
        """

        System.info('%s: analysing report' % self.name)

        self.tool = GNAThub.Tool(self.name)
        self.log.debug('Parse report: %s' % self.report)

        if not os.path.exists(self.report):
            self.exec_status = GNAThub.EXEC_FAILURE
            System.error('%s: no report found' % self.name)
            return

        try:
            with open(self.report, 'r') as output:
                lines = output.readlines()
                total = len(lines)

                for index, line in enumerate(lines, start=1):
                    self.log.debug('Parse line: %s' % line)

                    match = self._MESSAGE.match(line)

                    if match:
                        self.log.debug('Matched: %s' % str(match.groups()))
                        self.__parse_line(match)

                    System.progress(index, total, new_line=(index == total))

            self.exec_status = GNAThub.EXEC_SUCCESS

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAILURE
            System.error('%s: error: %s' % (self.name, ex))

    def __parse_line(self, regex):
        """Parses a GNATcheck message line and add the message to the current
        database session.

        Retrieves following information:
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
        base = regex.group('file')
        src = GNAThub.Project.source_file(base)
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
        rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
        message = GNAThub.Message(rule, msg)
        resource = GNAThub.Resource.get(src)

        if resource:
            resource.add_message(message, int(line), int(col_begin))
