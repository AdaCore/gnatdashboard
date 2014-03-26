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
module and load it as part of the GNAThub default execution.
"""

import re

# pylint: disable=F0401
# Disable: Unable to import '{}'
from _gnat import SLOC_PATTERN

import GNAThub
from GNAThub import Log, db


class GNATprove(GNAThub.Plugin):
    """GNATprove plugin for GNAThub.

    Configures and executes GNATprove, then analyzes its output.
    """

    TOOL_NAME = 'GNATprove'

    SEVERITIES = {'info': 'INFO', 'warning': 'MINOR', 'error': 'MAJOR'}

    # Regex to identify line that contains message
    MSG_PATTERN = \
        '%s:(:\s(?P<severity>[a-z]+))?:\s(?P<message>.+)' % SLOC_PATTERN
    RULE_PATTERN = ' \[(?P<rule>[a-z_]+)\]$'

    MSG_RE = re.compile(MSG_PATTERN)
    RULE_RE = re.compile(RULE_PATTERN)

    def __init__(self):
        super(GNATprove, self).__init__()
        self.tool = None

    def display_command_line(self):
        """Inherited."""

        return ' '.join(['-P', GNAThub.Project.name()])

    def __cmd_line(self):
        """Creates GNATcheck command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnatprove', '-P', GNAThub.Project.path(),
                '-j%d' % GNAThub.jobs()]

    def execute(self):
        """Executes the GNATprove.

        GNATprove.postprocess() will be called upon process completion.
        """

        Log.info('%s.run %s' % (self.fqn, self.display_command_line()))
        proc = GNAThub.Run(self.name, self.__cmd_line())
        self.postprocess(proc.status, proc.output())

    def postprocess(self, exit_code, logfile):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAILURE: on any error
        """

        if exit_code != 0:
            self.exec_status = GNAThub.EXEC_FAILURE
            return

        self.__parse_output(logfile)

    def __parse_output(self, logfile):
        """Parses GNATprove output file (logs recorded during execution).

        Identifies two types of messages with different format:
            - standard messages.
            - messages for package instantiations.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful analysis
            GNAThub.EXEC_FAILURE: on any error
        """

        self.tool = GNAThub.Tool(self.name)

        try:
            with open(logfile, 'r') as output:
                for line in output.readlines():
                    match = self.MSG_RE.match(line)
                    if match:
                        self.__parse_line(match)

            self.session.commit()
            self.exec_status = GNAThub.EXEC_SUCCESS

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAILURE
            Log.error('%s: failed to parse output: %s' % (self.fqn, logfile))
            Log.error(str(ex))

    def __parse_line(self, regex):
        """Parses a GNATprove message line and adds it to the database.

        Extracts the following information:
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
            #       ('nose_gear.adb', '140', '44', 'info',
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

        except KeyError:
            Log.warn('Unknown severity: %s' % severity)

    def __add_message(self, src, line, col_begin, rule_id, msg):
        """Registers a new message in the database.

        PARAMETERS
            :param src: The source file containing the message.
            :type src: A string, representing the full path to a file.
            :param line: A line from that source file.
            :type line: A string.
            :param col_begin: The starting column in the line.
            :type col_begin: A string.
            :param rule_id: The GNAThub rule ID.
            :type rule_id: A string.
            :param msg: The message to record.
            :type msg: A string.
        """

        rule = GNAThub.Rule(rule_id, rule_id, db.RULE_KIND, self.tool)
        message = GNAThub.Message(rule, msg)

        resource = GNAThub.Resource.get(src)

        if resource:
            resource.add_message(message, int(line), int(col_begin))
