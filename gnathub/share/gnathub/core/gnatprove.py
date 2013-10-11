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

"""GNAThub plug-in for the GNATprove command-line tool.

It exports the GNATprove Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import GNAThub

import re

from _gnat import PostProcessProtocol, SLOC_PATTERN

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage


class GNATprove(GNAThub.Plugin):
    """GNATprove plugin for GNAThub.

    Configures and executes GNATprove, then analizes the output.
    """

    TOOL_NAME = 'GNATprove'

    SEVERITIES = {'info': 'INFO', 'warning': 'MINOR', 'error': 'MAJOR'}

    # Regex to identify line that contains message
    MSG_PATTERN = \
        '%s:(:\s(?P<severity>[a-z]+))?: (?P<message>.+)' % SLOC_PATTERN
    RULE_PATTERN = ' \[(?P<rule>[a-z_]+)\]$'

    MSG_RE = re.compile(MSG_PATTERN)
    RULE_RE = re.compile(RULE_PATTERN)

    def __init__(self):
        super(GNATprove, self).__init__()

        self.process = GNAThub.Process(self.name, self.__cmd_line(),
                                       PostProcessProtocol(self))

    def display_command_line(self):
        """Inherited."""

        cmdline = super(GNATprove, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.append('--show-tag')

        return cmdline

    def __cmd_line(self):
        """Creates GNATcheck command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnatprove', '-P', GNAThub.project.path(), '--show-tag']

    def execute(self):
        """Executes the GNATprove.

        GNATprove.postprocess() will be called upon process completion.
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

        if exit_code != 0:
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            self.exec_status = GNAThub.EXEC_FAIL
            return

        self.__parse_output()

    def __parse_output(self):
        """Parses GNATprove output file (logs recorded during execution).

        Identifies two types of mmessages with different format:
            - standard messages.
            - messages for package instanciations.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful analysis
            GNAThub.EXEC_FAIL: on any error
        """

        self.tool = dao.save_tool(self.session, self.name)

        try:
            with open(self.logs(), 'r') as output:
                for line in output.readlines():
                    match = self.MSG_RE.match(line)
                    if match:
                        self.__parse_line(match)

            self.session.commit()
            self.exec_status = GNAThub.EXEC_SUCCESS

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: failed to parse output: %s' % (self.name,
                                                          self.logs()))
            Log.error(str(ex))

    def __parse_line(self, regex):
        """Parses a GNATprove message line and adds it to the database.

        Extracts the following informations:
            - source basename,
            - line in source,
            - rule identification,
            - message description

        PARAMETERS
            :param regex: the result of the MSG_RE regex.
            :type regex: a regex result.
        """

        # The following Regex results are explained using this example.
        # 'nose_gear.adb:144:50: info: overflow check proved [overflow_check]'

        try:
            # Extract each component from the message:
            #       ('nose_gear.adb', '140', '44', ' info',
            #        'overflow check proved [overflow_check]')
            src = regex.group('file')
            line = regex.group('line')
            column = regex.group('column')
            severity = regex.group('severity')
            message = regex.group('message')

            severity = self.SEVERITIES[severity if severity else 'error']

            # Extract the rule from the message: ('overflow_check')
            match = self.RULE_RE.match(message)
            rule = match.group('rule')

            # Generates the rule ID
            rule_id = '%s__%s' % (severity, rule)

            self.__add_message(src, line, column, rule_id, message)

        except IndexError:
            Log.warn('Unexpected message format: %s:%s' % (src, line))
            Log.warn(line)

        except KeyError:
            Log.warn('Unknown severity: %s' % severity)

    def __add_message(self, src, line, col_begin, rule_id, msg):
        """Registers a new message in the database.

        PARAMETERS
            :param src: The source file containing the message.
            :type src: A string.
            :param line: A line from that source file.
            :type line: A string.
            :param col_begin: The starting column in the line.
            :type col_begin: A string.
            :param rule_id: The GNAThub rule ID.
            :type rule_id: A string.
            :param msg: The message to record.
            :type msg: A string.
        """

        rule = dao.get_or_create_rule(self.session, self.tool,
                                      db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line)

        if line:
            line_message = LineMessage(col_begin=col_begin)
            line_message.message = Message(msg, rule)

            # pylint: disable=E1103
            # Disable "Module {} has no member {}" error
            line.messages.append(line_message)

        else:
            Log.warn('%s: file not found: %s' % (self.name, src))
            Log.warn('%s: skipping message: %s' % (self.name, msg))
