##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                     Copyright (C) 2013-2014, AdaCore                     ##
##                                                                          ##
## This is free software;  you can redistribute it  and/or modify it  under ##
## terms of the  GNU General Public License as published  by the Free Soft- ##
## ware  Foundation;  either version 3,  or (at your option) any later ver- ##
## sion.  This software is distributed in the hope  that it will be useful, ##
## but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License for  more details.  You should have  received  a copy of the GNU ##
## General  Public  License  distributed  with  this  software;   see  file ##
## COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy ##
## of the license.                                                          ##
##                                                                          ##
##############################################################################

"""GNAThub plug-in for the GNATprove command-line tool.

It exports the GNATprove Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import re

from _gnat import SLOC_PATTERN

import GNAThub


class GNATprove(GNAThub.Plugin):
    """GNATprove plugin for GNAThub.

    Configures and executes GNATprove, then analyzes its output.

    """

    SEVERITIES = {'info': 'INFO', 'warning': 'MINOR', 'error': 'MAJOR'}

    # Regex to identify line that contains message
    MSG_PATTERN = \
        r'%s:(:\s(?P<severity>[a-z]+))?:\s(?P<message>.+)' % SLOC_PATTERN
    RULE_PATTERN = r' \[(?P<rule>[a-z_]+)\]$'

    MSG_RE = re.compile(MSG_PATTERN)
    RULE_RE = re.compile(RULE_PATTERN)

    def __init__(self):
        super(GNATprove, self).__init__()
        self.tool = None

    @staticmethod
    def __cmd_line():
        """Creates GNATcheck command line arguments list.

        :returns: list[str]

        """

        return ['gnatprove', '-P', GNAThub.Project.path(),
                '-j%d' % GNAThub.jobs()]

    def execute(self):
        """Executes GNATprove.

        :meth:``postprocess()`` is called upon process completion.

        """

        proc = GNAThub.Run(self.name, GNATprove.__cmd_line())
        self.postprocess(proc.status, proc.output())

    def postprocess(self, exit_code, logfile):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error

        """

        if exit_code != 0:
            self.exec_status = GNAThub.EXEC_FAILURE
            return

        self.__parse_output(logfile)

    def __parse_output(self, logfile):
        """Parses GNATprove output file (logs recorded during execution).

        Identifies two types of messages with different format:

            * standard messages
            * messages for package instantiations

        Sets the exec_status property according to the success of the
        analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful analysis
            * ``GNAThub.EXEC_FAILURE``: on any error

        """

        self.tool = GNAThub.Tool(self.name)

        try:
            with open(logfile, 'r') as output:
                for line in output.readlines():
                    match = self.MSG_RE.match(line)
                    if match:
                        self.__parse_line(match)

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to parse GNATprove output')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS

    def __parse_line(self, regex):
        """Parses a GNATprove message line and adds it to the database.

        Extracts the following information:

            * source basename
            * line in source
            * rule identification
            * message description

        :param re.RegexObject regex: The result of the ``MSG_RE`` regex.

        """

        # The following Regex results are explained using this example.
        # 'nose_gear.adb:144:50: info: overflow check proved [overflow_check]'

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

    # pylint: disable=too-many-arguments
    def __add_message(self, src, line, col_begin, rule_id, msg):
        """Registers a new message in the database.

        :param str src: The source file containing the message.
        :param str line: A line from that source file.
        :param str col_begin: The starting column in the line.
        :param str rule_id: The GNAThub rule ID.
        :param str msg: The message to record.

        """

        rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
        message = GNAThub.Message(rule, msg)

        resource = GNAThub.Resource.get(src)

        if resource:
            resource.add_message(message, int(line), int(col_begin))
