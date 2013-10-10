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

"""GNAThub plug-in for the GNATmetric command-line tool.

It exports the GNATmetric Python class which implements the GNAThub.Plugin
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default excecution.
"""

import GNAThub
import GNAThub.project

import os

from _gnat import GNATToolProgressProtocol

from GNAThub import Log
from GNAThub import dao, db
from GNAThub.db import Message

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError


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
                                       GNATToolProgressProtocol(self))

    def __cmd_line(self):
        """Creates GNATmetric command line arguments list.

        RETURNS
            :rtype: a list of string
        """

        return ['gnat', 'metric', '-ox', self.report,
                '-P', GNAThub.project.path(), '-U']

    def execute(self):
        """Executes the GNATmetric.

        GNATmetric.postprocess() will be called upon process completion.
        """

        self.process.execute()

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output XML report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAIL: on any error
        """

        if exit_code not in GNATmetric.VALID_EXIT_CODES:
            Log.error('%s: execution failed' % self.name)
            Log.error('%s: see log file: %s' % (self.name, self.logs()))
            self.exec_status = GNAThub.EXEC_FAIL
            return

        self.__parse_xml_report()

    def display_command_line(self):
        """Inherited."""

        cmdline = super(GNATmetric, self).display_command_line()
        cmdline.extend(['-P', GNAThub.project.name()])
        cmdline.extend(['-o', os.path.relpath(self.report)])

        return cmdline

    def __parse_xml_report(self):
        """Parses GNATmetric XML report and save data to the database.

        Sets the exec_status property according to the success of the
        analysis:

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

                        # pylint: disable=E1103
                        # Disable "Module {} has no member {}" error
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
            self.exec_status = GNAThub.EXEC_SUCCESS

            Log.debug('%s: all objects commited to database' % self.name)

        except ParseError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: unable to parse XML report' % self.name)
            Log.error('%s:%s:%s - :%s' % (ex.filename, ex.lineno, ex.text,
                                          ex.msg))

        except IOError as ex:
            self.exec_status = GNAThub.EXEC_FAIL
            Log.error('%s: unable to parse XML report' % self.name)
            Log.error(str(ex))
