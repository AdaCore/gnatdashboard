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

import GNAThub
import GNAThub.project

import os
import re

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError


class _GNATmetricProtocol(GNAThub.LoggerProcessProtocol):
    REMAINING = re.compile('^Units remaining: (?P<count>[0-9]+)')

    def __init__(self, gnatmetric):
        GNAThub.LoggerProcessProtocol.__init__(self, gnatmetric)
        self.total = None

    def errReceived(self, data):
        GNAThub.LoggerProcessProtocol.errReceived(self, data)

        match = self.REMAINING.match(data)

        if match:
            count = int(match.group('count'))

            if self.total is None:
                self.total = count
                Log.progress(1, self.total)
            else:
                Log.progress(self.total - count, self.total)

    def processEnded(self, reason):
        GNAThub.LoggerProcessProtocol.processEnded(self, reason)

        Log.progress(self.total, self.total, new_line=True)

        self.plugin._postprocess(self.exit_code)
        self.plugin.ensure_chain_reaction()


class GNATmetric(GNAThub.Plugin):
    """GNATmetric plugin for GNAThub.
    """

    TOOL_NAME = 'GNATmetric'
    REPORT = 'metrix.xml'

    # GNATmetric exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        """Instance contsructor."""

        super(GNATmetric, self).__init__()

        self.report = os.path.join(GNAThub.project.object_dir(), self.REPORT)

        self.process = GNAThub.Process(self.name, self.__cmd_line(),
                                       _GNATmetricProtocol(self))

    def __cmd_line(self):
        """Creates GNATmetric command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnat', 'metric', '-ox', self.report,
                '-P', GNAThub.project.path(), '-U']

    def execute(self):
        """Executes the GNATmetric.

        GNATcheck._postprocess() will be called upon process completion.
        """

        self.process.execute()

    def _postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output XML report on
        success.

        RETURNS
            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code not in GNATmetric.VALID_EXIT_CODES:
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            return GNAThub.EXEC_FAIL

        return self.__parse_xml_report()

    def display_command_line(self):
        cmdline = super(GNATmetric, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(self.report)])
        return cmdline

    def __parse_xml_report(self):
        """Parses GNATmetric XML report and save data to the database.

        RETURNS
            GNAThub.EXEC_SUCCESS: if transaction have been comitted to database
            GNAThub.EXEC_FAIL: if error happened while parsing the xml report
        """

        Log.info('gnathub analyse %s' % os.path.relpath(self.report))

        Log.debug('%s: storing tool in database' % self.name)
        tool = dao.save_tool(self.session, self.name)

        Log.debug('%s: parsing XML report: %s' % (self.name, self.report))

        try:
            tree = ElementTree.parse(self.report)

            # Fetch all files
            files = tree.findall('./file')
            total = len(files)

            for index, file_node in enumerate(files, start=1):
                resource = dao.get_file(self.session,
                                        file_node.attrib.get('name'))
                # Save file level metrics
                if resource:
                    for metric in file_node.findall('./metric'):
                        name = metric.attrib.get('name')
                        rule = dao.get_or_create_rule(self.session, tool,
                                                      db.METRIC_KIND, name)
                        resource.messages.append(Message(metric.text, rule))
                else:
                    Log.warn('File not found, skipping all messages from: %s' %
                             file_node.attrib.get('name'))
                    continue

                # Save unit level metric
                for unit in file_node.findall('.//unit'):
                    for metric in unit.findall('./metric'):
                        pass
                        # /!\ Not handle for now: to be done /!\
                        # File --> file_node.attrib.get('name'),
                        # Entity Line --> unit.attrib.get('line'),
                        # Entity name --> unit.attrib.get('name'),
                        # Entity Col --> unit.attrib.get('col'),
                        # Metric name --> metric.attrib.get('name'),
                        # Metric value --> metric.text)

                Log.progress(index, total, new_line=(index == total))

            self.session.commit()

            Log.debug('%s: all objects commited to database' % self.name)
            return GNAThub.EXEC_SUCCESS

        except ParseError as e:
            Log.error('%s: unable to parse XML report' % self.name)
            Log.error('%s:%s:%s - :%s' % (e.filename, e.lineno, e.text, e.msg))
            return GNAThub.EXEC_FAIL

        except IOError as e:
            Log.error('%s: unable to parse XML report' % self.name)
            Log.error(e)
            return GNAThub.EXEC_FAIL
