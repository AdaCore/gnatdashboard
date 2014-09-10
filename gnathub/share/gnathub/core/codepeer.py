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

"""GNAThub plug-in for the CodePeer command-line tool.

It exports the CodePeer Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import csv
import os
import os.path

import GNAThub
from GNAThub import Console


class CodePeer(GNAThub.Plugin):
    """CodePeer plugin for GNAThub

    Configures and executes CodePeer, then analyzes the output.

    """

    def __init__(self):
        super(CodePeer, self).__init__()

        self.tool = None

        output = '%s.csv' % GNAThub.Project.name().lower()
        self.csv_report = os.path.join(GNAThub.Project.object_dir(),
                                       'codepeer', output)

        self.report = os.path.join(GNAThub.Project.object_dir(),
                                   '%s.out' % self.name)

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
        """Creates CodePeer command line arguments list.

        :return: The CodePeer command line.
        :rtype: list[str]

        """
        return ['codepeer', '-update-scil',
                '-P', GNAThub.Project.path(), '-jobs', str(GNAThub.jobs())
                ] + GNAThub.Project.scenario_switches()

    @staticmethod
    def __msg_reader_cmd_line():
        """Creates CodePeer Message Reader command line arguments list.

        :return: The CodePeer message reader command line.
        :rtype: list[str]

        """

        msg_dir = os.path.join(GNAThub.Project.object_dir(), 'codepeer',
                               '%s.output' % GNAThub.Project.name().lower())

        return ['codepeer_msg_reader', '-csv', msg_dir]

    def execute(self):
        """Executes CodePeer.

        :meth:`execute_msg_reader()` is called upon process completion.

        """

        proc = GNAThub.Run(self.name, CodePeer.__cmd_line())

        if proc.status:
            return

        self.execute_msg_reader()

    def execute_msg_reader(self):
        """Executes CodePeer Message Reader.

        :meth:`postprocess()` is called upon process completion.

        """

        self.info('collect results with msg_reader')
        proc = GNAThub.Run('msg_reader', CodePeer.__msg_reader_cmd_line(),
                           out=self.csv_report)

        self.postprocess(proc.status)

    def postprocess(self, exit_code):
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

        self.__parse_csv_report()

    def __parse_csv_report(self):
        """Parses CodePeer output CSV report.

        Sets the exec_status property according to the success of the
        analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error

        """

        self.info('analyse CSV report')
        self.tool = GNAThub.Tool(self.name)

        self.log.debug('parse report: %s', self.csv_report)

        if not os.path.exists(self.csv_report):
            self.exec_status = GNAThub.EXEC_FAILURE
            self.error('no report found')
            return

        with open(self.csv_report, 'rb') as report:
            # Compute the total number of lines for progress report (-1 because
            # the first line in irrelevant to the analysis).
            total = len(report.readlines()) - 1

            # Reset the read cursor to the first byte
            report.seek(0)

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
                    # [File, Line, Column, Category, New?, Review?, Ranking,
                    #  Kind, Message, Classification, CWE]

                    source = record[0]
                    line = record[1]
                    column = record[2]
                    rule = record[3]
                    severity = record[6]
                    category = record[7]
                    message = record[8]

                    # See L919-022: duplicated rules for priorities and
                    # categories in SonarQube Rule Repository.
                    # ???: Remove this work-around once fixed in SonarQube.
                    category = '%s__%s' % (severity.upper(), category.upper())

                    self.__add_message(source, line, column, rule, message,
                                       category)

                    Console.progress(index, total, new_line=(index == total))

            except csv.Error as why:
                self.exec_status = GNAThub.EXEC_FAILURE
                self.log.exception('failed to parse CodePeer CSV report')

                self.error('%s (%s:%d)' %
                           (why, os.path.basename(self.csv_report), total))

            else:
                self.__do_bulk_insert()
                self.exec_status = GNAThub.EXEC_SUCCESS

    # pylint: disable=too-many-arguments
    def __add_message(self, src, line, column, rule_id, msg, category):
        """Adds CodePeer message to current session database.

        :param str src: Message source file.
        :param str line: Message line number.
        :param str column: Message column number.
        :param str rule_id: Message's rule identifier.
        :param str msg: Description of the message.
        :param str category: The category of the message.

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
            message = GNAThub.Message(rule, msg, cat)
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
