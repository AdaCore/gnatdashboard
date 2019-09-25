# GNAThub (GNATdashboard)
# Copyright (C) 2013-2017, AdaCore
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

"""GNAThub plug-in for the CodePeer command-line tool.

It exports the CodePeer class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import collections
import csv
import os
import os.path

import GNAThub
from GNAThub import Console, Plugin, Reporter, Runner, ToolArgsPlaceholder


class CodePeer(Plugin, Runner, Reporter):
    """CodePeer plugin for GNAThub.

    Configures and executes CodePeer, then analyzes the output.
    """

    CODEPEER_TO_RANKING = {
      'annotation': GNAThub.RANKING_ANNOTATION,
      'info': GNAThub.RANKING_INFO,
      'low':  GNAThub.RANKING_LOW,
      'medium': GNAThub.RANKING_MEDIUM,
      'high': GNAThub.RANKING_HIGH
      }

    @property
    def output_dir(self):
        """Return the path to the directory where to generate the Codepeer files

        :return: the full path to the output directory
        :rtype: str
        """

        return os.path.join(GNAThub.Project.object_dir(), self.name)

    def __init__(self):
        super(CodePeer, self).__init__()

        self.tool = None

        self.csv_report = os.path.join(
             GNAThub.Project.object_dir(), 'codepeer',
             '{}.csv'.format(GNAThub.Project.name().lower()))

        # Map of rules (couple (name, rule): dict[str,Rule])
        self.rules = {}

        # Map of messages (couple (rule, message): dict[str,Message])
        self.messages = {}

        # Map of bulk data (couple (source, message_data): dict[str,list])
        self.bulk_data = collections.defaultdict(list)

    @staticmethod
    def __cmd_line():
        """Create CodePeer command line arguments list.

        :return: the CodePeer command line
        :rtype: collections.Iterable[str]
        """
        cmd_line = ['codepeer', '-P', GNAThub.Project.path(),
                    '-j%d' % GNAThub.jobs()]

        if GNAThub.u_process_all():
            cmd_line.extend(['-U'])

#  Keeping this for -U main switch implemntation
#        if GNAThub.u_main():
#            cmd_line.extend(['-U'])
#            cmd_line.extend([GNAThub.u_main()])

        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])

        return cmd_line + GNAThub.Project.scenario_switches()

    @staticmethod
    def __msg_reader_cmd_line(report):
        """Create CodePeer Message Reader command line arguments list.

        :return: the CodePeer message reader command line
        :rtype: collections.Iterable[str]
        """
        cmd_start = ['codepeer', '-P', GNAThub.Project.path()]
        if GNAThub.subdirs():
            cmd_start.extend(['--subdirs=' + GNAThub.subdirs()])

        cmd_start.extend([ToolArgsPlaceholder('codepeer')])

        dest = os.path.join(
             GNAThub.Project.object_dir(), 'codepeer',
             'codepeer_run')

        cmd_end = [
                   '-output-msg-only', '-csv', '-out', report,
                   ToolArgsPlaceholder('codepeer_msg_reader'),
                   '-db-info', dest
                  ]

        return cmd_start + GNAThub.Project.scenario_switches() + cmd_end

    def run(self):
        """Execute CodePeer.

        Sets the exec_status property according to the success of the
        execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status == 0 else GNAThub.EXEC_FAILURE

    def report(self):
        """Execute CodePeer message reader and parses the output.

        Sets the exec_status property according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        self.info('extract results with msg_reader to %s' % self.csv_report)
        proc = GNAThub.Run(
            self.output_dir, self.__msg_reader_cmd_line(self.csv_report))

        if proc.status != 0:
            return GNAThub.EXEC_FAILURE

        self.info('analyse CSV report form %s' % self.csv_report)
        self.tool = GNAThub.Tool(self.name)

        self.log.debug('parse report: %s', self.csv_report)

        if not os.path.isfile(self.csv_report):
            self.error('no report found')
            return GNAThub.EXEC_FAILURE

        with open(self.csv_report, 'rb') as report:
            # Compute the total number of lines for progress report (-1 because
            # the first line in irrelevant to the analysis).
            index, total = 0, len(report.readlines()) - 1

            # Reset the read cursor to the first byte
            report.seek(0)

            # Create the tag "New" for new CodePeer messages
            added_tag = GNAThub.Property('codepeer:added', 'Added')
            removed_tag = GNAThub.Property('codepeer:removed', 'Removed')
            unchanged_tag = GNAThub.Property('codepeer:unchanged', 'Unchanged')

            try:
                # Parse the file and drop the first line (containing the
                # columns name).
                reader = csv.reader(report, quotechar='\"')

                # Drop the first line (containing the columns name)
                header = reader.next()
                self.log.debug('drop header line: %s', header)

                # Iterate over each relevant record
                for index, record in enumerate(reader, start=1):
                    self.log.debug('parse record: %r', record)

                    # Each row is a list of strings:
                    #
                    #   File, Line, Column, Category, History, Has_Review,
                    #   Ranking, Kind, Message, Classification, CWE, Checks,
                    #   Primary_Checks, Subp, Timestamp, Approved By, Comment,
                    #   Message_Id
                    (
                        source, line, column, rule, history, has_review,
                        severity, category, message, classification, cwe,
                        checks, pchecks, subp, timestamp, app_by, comment,
                        message_id
                    ) = record[:18]

                    if not severity or severity == 'suppressed':
                        # Some versions of codepeer report an empty severity
                        # for suppressed messages: map this to 'info'.
                        severity = 'info'

                    rule_id = rule.lower()
                    self.__add_message(
                        source, line, column, rule_id, message, severity,
                        message_id,
                        [added_tag if history == 'added' else
                         (removed_tag if history == 'removed' else
                          unchanged_tag)]
                    )

                    if index % 100 == 1 or index == total:
                        Console.progress(
                            index, total, new_line=(index == total))

            except csv.Error as why:
                self.log.exception('failed to parse CSV report')
                self.error('%s (%s:%d)' % (
                    why, os.path.basename(self.csv_report), index))
                return GNAThub.EXEC_FAILURE

            else:
                self.__do_bulk_insert()
                return GNAThub.EXEC_SUCCESS

    def __get_ranking(self, severity):
        """Get corresponding ranking for a given severity

        :param str severity: message severity string value
        :param str ranking: corresponding integer value
        """

        sev = severity.lower().replace(' ', '')
        return self.CODEPEER_TO_RANKING.get(sev, GNAThub.RANKING_UNSPECIFIED)

    def __add_message(self, src, line, column, rule_id, msg, category,
                      tool_msg_id, properties):
        """Add CodePeer message to current session database.

        :param str src: message source file
        :param str line: message line number
        :param str column: message column number
        :param str rule_id: message rule identifier
        :param str msg: description of the message
        :param str category: the category of the message
        :param str tool_msg_id: the original id of the message
        :param properties: the message properties
        :type properties: collections.Iterable[GNAThub.Property] or None
        """

        # Get message ranking value
        ranking = self.__get_ranking(category)

        # Cache the rules
        if rule_id in self.rules:
            rule = self.rules[rule_id]
        else:
            rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
            self.rules[rule_id] = rule

        # Get message id from string
        msg_id = 0
        if tool_msg_id and tool_msg_id.strip().isdigit():
            msg_id = int(tool_msg_id)

        # Cache the messages
        if (rule, msg, ranking, msg_id) in self.messages:
            message = self.messages[(rule, msg, ranking, msg_id)]
        else:
            message = GNAThub.Message(rule, msg, ranking, msg_id, properties)
            self.messages[(rule, msg, ranking, msg_id)] = message

        # Add the message to the given resource
        self.bulk_data[src].append(
                [message, int(line), int(column), int(column)])

    def __do_bulk_insert(self):
        """Insert the codepeer messages in bulk on each resource."""

        # List of resource messages suitable for tool level bulk insertion
        resources_messages = []

        for src in self.bulk_data:
            base = GNAThub.Project.source_file(os.path.basename(src))
            resource = GNAThub.Resource.get(base)

            if resource:
                resources_messages.append([resource, self.bulk_data[src]])

        self.tool.add_messages(resources_messages, [])
