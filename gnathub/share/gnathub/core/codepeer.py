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

import csv
import os
import os.path

import GNAThub
from GNAThub import Console, Plugin, Reporter, Runner, ToolArgsPlaceholder


class CodePeer(Plugin, Runner, Reporter):
    """CodePeer plugin for GNAThub.

    Configures and executes CodePeer, then analyzes the output.
    """

    def __init__(self):
        super(CodePeer, self).__init__()

        self.tool = None

        self.csv_report = os.path.join(
            GNAThub.Project.object_dir(), 'codepeer',
            '{}.csv'.format(GNAThub.Project.name().lower()))

        # Map of rules (couple (name, rule): dict[str,Rule])
        self.rules = {}

        # Map of categories (couple (name, category): dict[str,Category])
        self.categories = {}

        # Map of messages (couple (rule, message): dict[str,Message])
        self.messages = {}

        # Map of bulk data (couple (source, message_data): dict[str,list])
        self.bulk_data = {}

    @staticmethod
    def __cmd_line():
        """Create CodePeer command line arguments list.

        :return: the CodePeer command line
        :rtype: collections.Iterable[str]
        """
        return [
            'codepeer', '-P', GNAThub.Project.path(),
            '-jobs', str(GNAThub.jobs()), '-update-scil'
        ] + GNAThub.Project.scenario_switches()

    @staticmethod
    def __msg_reader_cmd_line():
        """Create CodePeer Message Reader command line arguments list.

        :return: the CodePeer message reader command line
        :rtype: collections.Iterable[str]
        """

        return [
            'codepeer', '-P', GNAThub.Project.path(),
            ToolArgsPlaceholder('codepeer')
        ] + GNAThub.Project.scenario_switches() + [
            '-output-msg-only', '-csv',
            ToolArgsPlaceholder('codepeer_msg_reader')
        ]

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

        self.info('clear existing results if any')
        GNAThub.Tool.clear_references(self.name)

        self.info('extract results with msg_reader')
        proc = GNAThub.Run(
            self.name, self.__msg_reader_cmd_line(), out=self.csv_report)

        if proc.status != 0:
            return GNAThub.EXEC_FAILURE

        self.info('analyse CSV report')
        self.tool = GNAThub.Tool(self.name)

        self.log.debug('parse report: %s', self.csv_report)

        if not os.path.exists(self.csv_report):
            self.error('no report found')
            return GNAThub.EXEC_FAILURE

        with open(self.csv_report, 'rb') as report:
            # Compute the total number of lines for progress report (-1 because
            # the first line in irrelevant to the analysis).
            total = len(report.readlines()) - 1

            # Reset the read cursor to the first byte
            report.seek(0)

            # Create the tag "New" for new CodePeer messages
            new_tag = GNAThub.Property('codepeer:is-new', 'New')
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
                    #   File, Line, Column, Category, New?, Review?, Ranking,
                    #   Kind, Message, Classification, CWE
                    (
                        source, line, column, rule, is_new, _, severity,
                        category, message
                    ) = record[:9]

                    if not severity or severity == 'suppressed':
                        # Some versions of codepeer report an empty severity
                        # for suppressed messages: map this to 'info'.
                        severity = 'info'

                    rule_id = rule.lower().partition(';')[0].replace(' ', '_')
                    self.__add_message(
                        source, line, column, rule_id, message, severity,
                        [new_tag if is_new == 'TRUE' else unchanged_tag]
                    )

                    Console.progress(index, total, new_line=(index == total))

            except csv.Error as why:
                self.log.exception('failed to parse CSV report')
                self.error('%s (%s:%d)' %
                           (why, os.path.basename(self.csv_report), total))
                return GNAThub.EXEC_FAILURE

            else:
                self.__do_bulk_insert()
                return GNAThub.EXEC_SUCCESS

    def __add_message(self, src, line, column, rule_id, msg, category,
                      properties):
        """Add CodePeer message to current session database.

        :param str src: message source file
        :param str line: message line number
        :param str column: message column number
        :param str rule_id: message rule identifier
        :param str msg: description of the message
        :param str category: the category of the message
        :param properties: the message properties
        :type properties: collections.Iterable[GNAThub.Property] or None
        """

        # Cache the rules
        if rule_id in self.rules:
            rule = self.rules[rule_id]
        else:
            rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
            self.rules[rule_id] = rule

        # cache the categories
        if category in self.categories:
            cat = self.categories[category]
        else:
            cat = GNAThub.Category(category)
            self.categories[category] = cat

        # cache the messages
        if (rule, msg, cat) in self.messages:
            message = self.messages[(rule, msg, cat)]
        else:
            message = GNAThub.Message(rule, msg, cat, properties)
            self.messages[(rule, msg, cat)] = message

        # Add the message to the given resource
        if src in self.bulk_data:
            self.bulk_data[src].append(
                [message, int(line), int(column), int(column)])
        else:
            self.bulk_data[src] = [
                [message, int(line), int(column), int(column)]]

    def __do_bulk_insert(self):
        """Insert the codepeer messages in bulk on each resource."""

        for src in self.bulk_data:
            base = GNAThub.Project.source_file(os.path.basename(src))
            resource = GNAThub.Resource.get(base)

            if resource:
                resource.add_messages(self.bulk_data[src])
