# GNAThub (GNATdashboard)
# Copyright (C) 2013-2019, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

"""GNAThub plug-in for the GNATcheck command-line tool.

It exports the GNATcheck class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import collections
import os
import re

from _gnat import SLOC_PATTERN

import GNAThub
from GNAThub import Console, Plugin, Reporter, Runner


class GNATcheck(Plugin, Runner, Reporter):
    """GNATcheck plugin for GNAThub.

    Configures and executes GNATcheck, then analyzes the output.
    """
    # Regex to identify lines that contain sections titles
    ''' Pattern to match common title like:
      1. Summary
      Format is [section_nb]. [section_title]
    '''
    _SECTION_TITLE_PATTERN = r'(?P<snumber>\d+(?:\.\d+)*)\. (?P<stitle>\w+)'

    # Regex to identify lines that contain messages
    ''' Pattern to match common rules like:
      p.adb:45:12: positional parameter association [Positional_Parameters]
      Format is [SLOC] [message] [rule_id]
    '''
    _RULE_PATTERN = r'(?P<message>.+)\s\[(?P<rule_id>[A-Za-z_:]+)\]$'

    ''' Pattern to match specific rules that are checked inside
        expanded generic instantiations
      p_g.adb:14:04 instance at p.ads:11:04:
      function returns unconstrained array [Unconstrained_Array_Returns]

        Ada allows unlimited depth of generic instantiations, so these rules
        can generate diagnoses with unlimited length of the SLOCs chain:

      p_g.adb:20:04 instance at p_g_g.ads:9:04 instance at p.ads:13:04:
      function returns unconstrained array [Unconstrained_Array_Returns]
    '''
    _RULE_PATTERN_INST = \
        (r'((?P<instance>[a-zA-Z-_.0-9 ]+:?([0-9]+):([0-9]+)?:?'
         r'\s))?(?P<message>.+)\s\[(?P<rule_id>[A-Za-z_]+):?'
         r'(?P<rule_info>[A-Za-z_:]+)?\]$')

    # Regular expression to match GNATcheck output and extract all relevant
    # information stored in it.
    _TITLE = re.compile(r'%s' % (_SECTION_TITLE_PATTERN))

    _MESSAGE = re.compile(r'%s:\s%s' % (SLOC_PATTERN, _RULE_PATTERN))

    _MESSAGE_INST = re.compile(
         r'%s:?\s%s' % (SLOC_PATTERN, _RULE_PATTERN_INST))

    # GNATcheck exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        super(GNATcheck, self).__init__()

        self.tool = None
        self.output = os.path.join(GNAThub.Project.object_dir(),
                                   '%s.out' % self.name)

        # Map of rules (couple (name, rule): dict[str,Rule])
        self.rules = {}

        # Map of messages (couple (rule, message): dict[str,Message])
        self.messages = {}

        # Map of bulk data (couple (source, message_data): dict[str,list])
        self.bulk_data = collections.defaultdict(list)

    def __cmd_line(self):
        """Create GNATcheck command line arguments list.

        :return: the GNATcheck command line
        :rtype: collections.Iterable[str]
        """

        cmd_line = [
            'gnatcheck', '--show-rule', '-o', self.output,
            '-P', GNAThub.Project.path()]

        if GNAThub.u_process_all():
            cmd_line.extend(['-U'])

#  Keeping this for later implemntation of -U main switch
#        if GNAThub.u_main():
#            cmd_line.extend(['-U'])
#            cmd_line.extend([GNAThub.u_main()])

        cmd_line.extend(['-j%d' % GNAThub.jobs()])

        cmd_line = cmd_line + GNAThub.Project.scenario_switches()

        if GNAThub.Project.target():
            cmd_line[0] = '{}-{}'.format(GNAThub.Project.target(), cmd_line[0])
        if GNAThub.Project.runtime():
            cmd_line.extend(('--RTS', GNAThub.Project.runtime()))
        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])
        return cmd_line

    def run(self):
        """Execute GNATcheck.

        Returns according to the success of the execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status in GNATcheck.VALID_EXIT_CODES else GNAThub.EXEC_FAILURE

    def report(self):
        """Parse GNATcheck output file report.

        Returns according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error

        Identify two type of messages with different format:

            * basic message
            * message for package instantiation
        """
        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        self.info('analyse report')

        self.tool = GNAThub.Tool(self.name)
        self.log.debug('parse report: %s', self.output)

        if not os.path.exists(self.output):
            self.error('no report found')
            return GNAThub.EXEC_FAILURE

        try:
            with open(self.output, 'r') as output:
                lines = output.readlines()
                total = len(lines)
                exempted_violation = False

                for index, line in enumerate(lines, start=1):
                    self.log.debug('parse line: %s', line)

                    # check if is a section title
                    matchTitle = self._TITLE.match(line)
                    if matchTitle:
                        stitle = matchTitle.group('stitle')
                        exempted_violation = stitle in ('Exempted', 'EXEMPTED')

                    # filter messages if occurs in exempted violation section
                    if not exempted_violation:
                        match = self._MESSAGE.match(line)
                        if match:
                            self.log.debug('matched: %s', str(match.groups()))
                            self.__parse_line(match)
                        else:
                            match2 = self._MESSAGE_INST.match(line)
                            if match2:
                                self.log.debug('matched 2: %s',
                                               str(match2.groups()))
                                self.__parse_line_inst(match2)

                    Console.progress(index, total, new_line=(index == total))

        except IOError as why:
            self.log.exception('failed to parse report')
            self.error('%s (%s:%d)' % (
                why, os.path.basename(self.output), total))
            return GNAThub.EXEC_FAILURE

        else:
            self.__do_bulk_insert()
            return GNAThub.EXEC_SUCCESS

    def __parse_line(self, regex):
        """Parse a GNATcheck message line.

        Adds the message to the current database session.

        Retrieves following information:

            * source basename
            * line in source
            * rule identification
            * message description

        :param re.RegexObject regex: the result of the _MESSAGE regex
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
        rule_id = regex.group('rule_id').lower()

        self.__add_message(src, line, column, rule_id, message)

    def __parse_line_inst(self, regex):
        """Parse a GNATcheck instance message line.

        Adds the formatted instance message to the current database session.

        Retrieves following information:

            * source basename
            * line in source
            * rule identification
            * instance + message description

        :param re.RegexObject regex: the result of the _MESSAGE_INST regex
        """

        # The following Regex results are explained using this example.
        # 'p_g.adb:20:04 instance at p_g_g.ads:9:04 instance at p.ads:13:04:
        #   function returns unconstrained array [Unconstrained_Array_Returns]'

        # Extract each component from the message:
        #     ('p_g.adb', '20', '04', 'instance at p_g_g.ads', '9', '04',
        #      'instance at p.ads:13:04: function returns unconstrained array',
        #      'Unconstrained_Array_Returns')

        base = regex.group('file')
        src = GNAThub.Project.source_file(base)
        line = regex.group('line')
        column = regex.group('column')
        instance = regex.group('instance')
        message = regex.group('message')
        rule_id = regex.group('rule_id').lower()

        # Build message including instance information
        new_msg = instance + message
        self.__add_message(src, line, column, rule_id, new_msg)

    def __add_message(self, src, line, column, rule_id, msg):
        """Add GNATcheck message to current session database.

        :param str src: Message source file.
        :param str line: Message line number.
        :param str column: Message column number.
        :param str rule_id: Message's rule identifier.
        :param str msg: Description of the message.
        """

        # Cache the rules
        if rule_id in self.rules:
            rule = self.rules[rule_id]
        else:
            rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
            self.rules[rule_id] = rule

        # Set predefined unspecified ranking for all GNATcheck messages
        ranking = GNAThub.RANKING_UNSPECIFIED

        # Cache the messages
        if (rule, msg, ranking) in self.messages:
            message = self.messages[(rule, msg, ranking)]
        else:
            message = GNAThub.Message(rule, msg, ranking)
            self.messages[(rule, msg, ranking)] = message

        # Add the message to the given resource
        self.bulk_data[src].append(
            [message, int(line), int(column), int(column)])

    def __do_bulk_insert(self):
        """Insert the gnatcheck messages in bulk on each resource."""

        # List of resource messages suitable for tool level bulk insertion
        resources_messages = []

        for src in self.bulk_data:
            base = GNAThub.Project.source_file(os.path.basename(src))
            resource = GNAThub.Resource.get(base)

            if resource:
                resources_messages.append([resource, self.bulk_data[src]])

        self.tool.add_messages(resources_messages, [])
