# GNAThub (GNATdashboard)
# Copyright (C) 2017-2021, AdaCore
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

"""GNAThub plug-in for the SPARK2014 command-line tool.

It exports the SPARK2014 class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

import collections
import json
import os
import os.path
import re

from GNAThub import Console, Plugin, Reporter, Runner

from _gnat import SLOC_PATTERN
from itertools import chain


class SPARK2014(Plugin, Runner, Reporter):
    """SPARK2014 plugin for GNAThub.

    Configures and executes GNATprove, then analyzes the output.
    """

    SPARK_TO_RANKING = {
      'info': GNAThub.RANKING_INFO,
      'low':  GNAThub.RANKING_LOW,
      'warning': GNAThub.RANKING_MEDIUM,
      'medium': GNAThub.RANKING_MEDIUM,
      'high': GNAThub.RANKING_HIGH
      }

    # Regex to identify lines that contain messages
    _MSG_PATTERN = \
        r'(?P<category>.+):\s(?P<message>.+)\[#(?P<msg_id>[0-9]+)\]$'

    # Regular expression to match SPARK output and extract all relevant
    # information stored in it.
    _MESSAGE = re.compile(r'%s:\s%s' % (SLOC_PATTERN, _MSG_PATTERN))

    def __init__(self):
        super(SPARK2014, self).__init__()

        if GNAThub.dry_run_without_project():
            return

        self.tool = None

        self.output_dir = os.path.join(
            GNAThub.Project.artifacts_dir(), 'gnatprove')
        self.output = os.path.join(
            GNAThub.Project.artifacts_dir(), 'gnatprove-gnathub.out')

        # Map of message ID (couple (filename, msg_id): dict[*])
        self.msg_ids = {}

        # Map of rules (couple (name, rule): dict[str,Rule])
        self.rules = {}

        # Map of messages (couple (rule, message): dict[str,Message])
        self.messages = {}

        # Map of bulk data (couple (source, message_data): dict[str,list])
        self.bulk_data = collections.defaultdict(list)

    @staticmethod
    def __cmd_line():
        """Create GNATprove command line arguments list.

        :return: the GNATprove command line
        :rtype: collections.Iterable[str]
        """

        cmd_line = ['gnatprove', '-P', GNAThub.Project.path()]
        if GNAThub.u_process_all():
            cmd_line.extend(['-U'])

#  Keeping this for later implementation of -U main switch
#        if GNAThub.u_main():
#            cmd_line.extend([GNAThub.u_main()])

        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])

        cmd_line.extend(['--report=all', '-j', str(GNAThub.jobs())])
        return cmd_line + GNAThub.Project.scenario_switches()

    @staticmethod
    def __msg_reader_cmd_line():
        """Create GNATprove Message Reader command line arguments list.

        :return: the GNATprove message reader command line
        :rtype: collections.Iterable[str]
        """

        cmd_line = ['gnatprove', '-P', GNAThub.Project.path()]
        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])

        cmd_line.extend(['--report=all', '-j', str(GNAThub.jobs()),
                         '--output-msg-only', '--ide-progress-bar'])
        return cmd_line + GNAThub.Project.scenario_switches()

    def run(self):
        """Execute GNATprove.

        Sets the exec_status property according to the success of the
        execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status == 0 else GNAThub.EXEC_FAILURE

    def report(self):
        """Execute GNATprove message reader and parses the output.

        Sets the exec_status property according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error
        """
        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        self.info('extract results with msg_reader')
        proc = GNAThub.Run(
            self.output_dir, self.__msg_reader_cmd_line(), out=self.output)

        if proc.status != 0:
            return GNAThub.EXEC_FAILURE

        self.info('analyse report')
        self.tool = GNAThub.Tool(self.name)

        # Handle multiple object directories for a given project
        if GNAThub.Project.object_dirs():
            # If there are multiple object directories defined in the project
            # tree will pass here and will look for all .spark files that was
            # generated under the object dirs gnatprove's folder

            for obj_dir in GNAThub.Project.object_dirs():
                # Fetch all files in project object directories and retrieve
                # only .spark files from gnatprove folders

                gnatprove_dir = os.path.join(obj_dir, 'gnatprove')
                if os.path.isdir(gnatprove_dir):
                    self.log.debug('parse report: %s', gnatprove_dir)
                    self.__parse_spark_files(gnatprove_dir)

        else:
            self.log.debug('parse report: %s', self.output_dir)
            if not os.path.isdir(self.output_dir):
                self.error('no report found')
                return GNAThub.EXEC_FAILURE

            self.__parse_spark_files(self.output_dir)

        try:
            with open(self.output, 'r') as fdin:
                # Compute the total number of lines for progress report
                lines = fdin.readlines()
                index, total = 0, len(lines)

                for index, line in enumerate(lines, start=1):
                    self.log.debug('parse line: %r', line)
                    match = self._MESSAGE.match(line)

                    if match:
                        self.log.debug('matched: %s', str(match.groups()))
                        self.__parse_line(match)

                    Console.progress(index, total, new_line=(index == total))

        except IOError as why:
            self.log.exception('failed to parse GNATprove output')
            self.error('%s (%s:%d)' % (
                why, os.path.basename(self.output), total))
            return GNAThub.EXEC_FAILURE

        else:
            self.__do_bulk_insert()
            return GNAThub.EXEC_SUCCESS

    def __parse_spark_files(self, files_dir):
        """ Retrieve only .spark files from files_dir folder and parse them.

        Fill the msg_ids with the related information of "flow" and "proof"
        record generated for each parsed file, when these records are not empty

        :param files_dir: files folder PATH to be handled

        """
        for entry in os.listdir(files_dir):
            filename, ext = os.path.splitext(entry)
            if not ext == '.spark':
                continue

            self.log.debug('parse file: %s', entry)
            try:
                with open(os.path.join(files_dir, entry), 'r') as spark:
                    results = json.load(spark)
                    for record in chain(results['flow'], results['proof']):
                        if 'msg_id' not in record or 'file' not in record:
                            continue

                        self.log.debug('found record %s', json.dumps(record))

                        msg_id = record['msg_id']
                        filename = record['file']
                        self.msg_ids[(filename, msg_id)] = record

            except IOError as why:
                self.log.exception('failed to parse GNATprove .spark file')
                self.error('%s (%s:%s)' % (
                    why, os.path.basename(self.output), entry))

    def __parse_line(self, regex):
        """Parse a GNATprove message line.

        Adds the message to the current database session.

        Retrieves following information:

            * source basename
            * line in source
            * rule identification
            * message description

        :param re.RegexObject regex: the result of the _MESSAGE regex
        """

        filename = regex.group('file')
        src = GNAThub.Project.source_file(filename)
        line = regex.group('line')
        column = regex.group('column')
        message = regex.group('message')
        msg_id = regex.group('msg_id')
        category = regex.group('category').lower()

        record = self.msg_ids.get((filename, int(msg_id)))

        if record is None:
            self.log.warn(
                '%s: failed to get record for msg_id #%s', filename, msg_id)
            return

        rule_id = record['rule'].lower()
        self.__add_message(src, line, column, rule_id, message, category)

    def __get_ranking(self, severity):
        """Get corresponding ranking for a given severity

        :param str severity: message severity string value
        :param str ranking: corresponding integer value
        """

        sev = severity.lower().replace(' ', '')
        return self.SPARK_TO_RANKING.get(sev, GNAThub.RANKING_UNSPECIFIED)

    def __add_message(self, src, line, column, rule_id, msg, category):
        """Add GNATprove message to current session database.

        :param str src: message source file
        :param str line: message line number
        :param str column: message column number
        :param str rule_id: message rule identifier
        :param str msg: description of the message
        :param str category: the category of the message
        """

        # Get message ranking value
        ranking = self.__get_ranking(category)

        # Cache the rules
        if rule_id in self.rules:
            rule = self.rules[rule_id]
        else:
            rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
            self.rules[rule_id] = rule

        # Cache messages
        if (rule, msg, ranking) in self.messages:
            message = self.messages[(rule, msg, ranking)]
        else:
            message = GNAThub.Message(rule, msg, ranking)
            self.messages[(rule, msg, ranking)] = message

        # Add the message to the given resource
        self.bulk_data[src].append(
            [message, int(line), int(column), int(column)])

    def __do_bulk_insert(self):
        """Insert the spark messages in bulk on each resource."""

        # List of resource messages suitable for tool level bulk insertion
        resources_messages = []

        for src in self.bulk_data:
            base = GNAThub.Project.source_file(os.path.basename(src))
            resource = GNAThub.Resource.get(base)

            if resource:
                resources_messages.append([resource, self.bulk_data[src]])

        self.tool.add_messages(resources_messages, [])
