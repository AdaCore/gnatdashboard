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

"""GNAThub plug-in for the CodePeer command-line tool.

It exports the CodePeer Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import GNAThub
import GNAThub.project

import csv
import os
import re

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage


class _CodePeerProtocol(GNAThub.LoggerProcessProtocol):
    """A custom LoggerProcessProtocol that calls the plug-in's postprocess
    method then ensures plug-ins chaining.
    """

    SPLIT_MENTION = re.compile('^.* split into (?P<total>[0-9]+) parts.$',
                               re.MULTILINE)
    PROCESSING = re.compile('^partition (?P<current>[0-9]+) of [0-9]+.$',
                            re.MULTILINE)
    COMPLETED = re.compile('^(?P<count>[0-9]+) .scil file(s)? processed.$',
                           re.MULTILINE)
    WAITING_LOCK = re.compile('^Waiting for lock (?P<lock>.*)$', re.MULTILINE)

    def __init__(self, codepeer):
        """Instance constructor."""

        GNAThub.LoggerProcessProtocol.__init__(self, codepeer)

        self.total = None
        self.count = 0

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def errReceived(self, data):
        """Inherited."""

        GNAThub.LoggerProcessProtocol.errReceived(self, data)

        match = self.WAITING_LOCK.match(data)

        if match:
            Log.error('%s: Waiting for lock file: %s' %
                      (self.plugin.name, match.group('lock')))

        if self.total is None:
            # We cannot infer the number of partitions yet
            return

        match = self.COMPLETED.search(data)

        if match:
            self.count = self.count + 1
            Log.progress(self.count, self.total,
                         new_line=(self.count == self.total))
            Log.debug('Processed %s .scil' % match.group('count'))

        match = self.PROCESSING.match(data)

        if match:
            current = int(match.group('current'))

            Log.debug('Processing partition %d of %d' % (current, self.total))

            if current == 1:
                Log.progress(0, self.total)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def outReceived(self, data):
        """Inherited."""

        GNAThub.LoggerProcessProtocol.outReceived(self, data)

        match = self.SPLIT_MENTION.match(data)

        if match:
            self.total = int(match.group('total'))
            Log.debug('Found total partitions: %d' % self.total)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def processEnded(self, reason):
        """Inherited."""

        GNAThub.LoggerProcessProtocol.processEnded(self, reason)

        if self.total is None:
            Log.progress(1, 1, new_line=True)

        if self.exit_code != 0:
            Log.error('%s: execution failed' % self.plugin.name)
            Log.error('%s: see log file: %s' % (self.plugin.name,
                                                self.plugin.logs()))
            self.plugin.exec_status = GNAThub.EXEC_FAIL

            # Ensure that we don't break the plugin chain.
            self.plugin.ensure_chain_reaction()
            return

        self.plugin.execute_msg_reader()


class _CodePeerMsgReaderProtocol(GNAThub.ProcessProtocol):
    """A custom ProcessProtocol that redirects all output to the report file.
    """

    def __init__(self, codepeer):
        """Instance constructor."""

        GNAThub.ProcessProtocol.__init__(self, codepeer)
        self._fd = open(codepeer.report, 'w+')

        Log.debug('%s: will log to: %s' % (codepeer.name, codepeer.report))

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def outReceived(self, data):
        """Inherited."""

        GNAThub.ProcessProtocol.outReceived(self, data)
        self._fd.write(data)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def errReceived(self, data):
        """Inherited."""

        GNAThub.ProcessProtocol.errReceived(self, data)
        self._fd.write(data)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def processEnded(self, reason):
        """Inherited."""

        GNAThub.ProcessProtocol.processEnded(self, reason)

        self._fd.close()
        self.plugin.postprocess(self.exit_code)

        # Ensure that we don't break the plugin chain.
        self.plugin.ensure_chain_reaction()


class CodePeer(GNAThub.Plugin):
    """CodePeer plugin for GNAThub

    Configures and executes CodePeer, then analizes the output.
    """

    TOOL_NAME = 'CodePeer'
    REPORT = 'codepeer.csv'

    def __init__(self):
        """Instance constructor."""

        super(CodePeer, self).__init__()

        self.report = os.path.join(GNAThub.project.object_dir(), self.REPORT)

        self.codepeer = GNAThub.Process(self.name, self.__cmd_line(),
                                        _CodePeerProtocol(self))

        self.msg_reader = GNAThub.Process('msg_reader',
                                          self.__msg_reader_cmd_line(),
                                          _CodePeerMsgReaderProtocol(self))

    def display_command_line(self):
        """Inherited."""

        cmdline = super(CodePeer, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(self.report)])

        return cmdline

    def __cmd_line(self):
        """Creates CodePeer command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['codepeer', '-update-scil', '-level', '1',
                '-P', GNAThub.project.path()]

    def __msg_reader_cmd_line(self):
        """Creates CodePeer Message Reader command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        output = '%s.output' % GNAThub.project.name().lower()
        results = os.path.join(GNAThub.project.object_dir(), 'codepeer',
                               output)

        return ['codepeer_msg_reader', '-csv', results]

    def execute(self):
        """Executes the CodePeer.

        CodePeer.execute_msg_reader() will be called upon process completion.
        """

        self.codepeer.execute()

    def execute_msg_reader(self):
        """Executes the CodePeer Message Reader.

        CodePeer.postprocess() will be called upon process completion.
        """

        self.msg_reader.execute()

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code != 0:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            return

        self.__parse_csv_report()

    def __parse_csv_report(self):
        """Parses CodePeer output CSV report.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        Log.info('gnathub analyse %s' % os.path.relpath(self.report))

        Log.debug('%s: storing tool in database' % self.name)
        self.tool = dao.save_tool(self.session, self.name)

        Log.debug('%s: parsing CSV report: %s' % (self.name, self.report))

        if not os.path.exists(self.report):
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: no report found' % self.name)
            Log.error('%s: aborting analysis' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            return

        with open(self.report, 'rb') as report:
            # Compute the total number of lines for progress report (-1 because
            # the first line in irrelevant to the analysis).
            total = len(report.readlines()) - 1

            # Reset the read cursor to the first byte
            report.seek(0)

            # Parse the file and drop the first line (containing the columns
            # name).
            reader = csv.reader(report, quotechar='\"')

            try:
                # Drop the first line (containing the columns name)
                header = reader.next()
                Log.debug('Dropping header line: %s' % header)

                # Iterate over each relevant record
                for index, record in enumerate(reader, start=1):
                    Log.debug('Parsing record: %r' % record)

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

                    Log.progress(index, total, new_line=(index == total))

            except csv.Error as ex:
                Log.error('%s report analysis failed.' % self.name)
                Log.error('file %s, line %d: %s' % (report, total, ex))
                self.exec_status = GNAThub.EXEC_FAIL
                return

            self.session.commit()
            self.exec_status = GNAThub.EXEC_SUCCESS

    def __add_message(self, src, line, column, rule_id, msg, category):
        """Adds CodePeer message to current session database.

        Parameters:
            :param src: message source file
            :type src: a string
            :param line: message line number
            :type line: a string
            :param column: message column number
            :type column: a string
            :param rule_id: message's rule identifier
            :type rule_id: a string
            :param msg: description of the message
            :type msg: a string
        """

        rule = dao.get_or_create_rule(self.session, self.tool,
                                      db.RULE_KIND, rule_id)
        line = dao.get_or_create_line(self.session, src, line)
        category = dao.get_or_create_category(self.session, category)

        if not line:
            Log.warn('Skipping message: %s, file not found: %s' % (msg, src))
            return

        line_message = LineMessage(col_begin=column)
        line_message.message = Message(msg, rule, category)

        # pylint: disable=E1103
        # Disable "Module {} has no member {}" error
        line.messages.append(line_message)
