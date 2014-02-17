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

import csv
import os

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message, LineMessage


class CodePeer(GNAThub.Plugin):
    """CodePeer plugin for GNAThub

    Configures and executes CodePeer, then analizes the output.
    """

    REPORT = 'codepeer.out'
    TOOL_NAME = 'CodePeer'

    def __init__(self):
        """Instance constructor."""

        super(CodePeer, self).__init__()

        output = '%s.csv' % GNAThub.Project.name().lower()
        self.csv_report = os.path.join(GNAThub.Project.object_dir(),
                                       'codepeer', output)

        self.report = os.path.join(GNAThub.Project.object_dir(), self.REPORT)

    def display_command_line(self):
        """Inherited."""

        cmdline = ['-P', GNAThub.Project.name()]
        cmdline.extend(['-o', os.path.relpath(self.report)])

        return ' '.join(cmdline)

    def __cmd_line(self):
        """Creates CodePeer command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['codepeer', '-update-scil', '-level', '1',
                '-P', GNAThub.Project.path(), '-jobs', str(GNAThub.jobs())]

    def __msg_reader_cmd_line(self):
        """Creates CodePeer Message Reader command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        msg_dir = os.path.join(GNAThub.Project.object_dir(), 'codepeer',
                               '%s.output' % GNAThub.Project.name().lower())

        return ['codepeer_msg_reader', '-csv', msg_dir]

    def execute(self):
        """Executes the CodePeer.

        CodePeer.execute_msg_reader() will be called upon process completion.
        """

        Log.info('%s.run %s' % (self.fqn, self.display_command_line()))
        proc = GNAThub.Run(self.name, self.__cmd_line())

        if proc.status:
            return

        self.execute_msg_reader()

    def execute_msg_reader(self):
        """Executes the CodePeer Message Reader.

        CodePeer.postprocess() will be called upon process completion.
        """

        Log.info('%s.msg_reader' % self.fqn)
        proc = GNAThub.Run('msg_reader', self.__msg_reader_cmd_line(),
                           out=self.csv_report)

        self.postprocess(proc.status)

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
            return

        self.__parse_csv_report()

    def __parse_csv_report(self):
        """Parses CodePeer output CSV report.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        Log.info('%s.analyse %s' % (self.fqn,
                                    os.path.relpath(self.csv_report)))

        Log.debug('%s: storing tool in database' % self.fqn)
        self.tool = dao.save_tool(self.session, self.name)

        Log.debug('%s: parsing CSV report: %s' % (self.fqn, self.csv_report))

        if not os.path.exists(self.csv_report):
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: no report found, aborting.' % self.fqn)
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
                Log.error('%s: report analysis failed.' % self.fqn)
                Log.error('%s: file %s, line %d: %s' % (self.fqn, report,
                                                        total, ex))
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
