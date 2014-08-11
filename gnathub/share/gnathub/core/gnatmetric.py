##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                     Copyright (C) 2013-2014, AdaCore                     ##
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
module and load it as part of the GNAThub default execution.
"""

import os

import GNAThub
from GNAThub import Console

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError


class GNATmetric(GNAThub.Plugin):
    """GNATmetric plugin for GNAThub."""

    name = 'gnatmetric'

    # GNATmetric exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        super(GNATmetric, self).__init__()

        self.tool = None
        self.report = os.path.join(GNAThub.Project.object_dir(), 'metrix.xml')

    def __cmd_line(self):
        """Creates GNATmetric command line arguments list.

        :returns: list[str]

        """

        return ['gnatmetric', '-ox', self.report, '-P', GNAThub.Project.path(),
                '-U']

    def execute(self):
        """Executes the GNATmetric.

        GNATmetric.postprocess() will be called upon process completion.

        """

        proc = GNAThub.Run(self.name, self.__cmd_line())
        self.postprocess(proc.status)

    def postprocess(self, exit_code):
        """Postprocesses the tool execution: parse the output XML report on
        success.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: on successful execution and analysis
            GNAThub.EXEC_FAILURE: on any error

        """

        if exit_code not in GNATmetric.VALID_EXIT_CODES:
            self.exec_status = GNAThub.EXEC_FAILURE
            return

        self.__parse_xml_report()

    def __parse_xml_report(self):
        """Parses GNATmetric XML report and save data to the database.

        Sets the exec_status property according to the success of the
        analysis:

            GNAThub.EXEC_SUCCESS: transaction have been committed to database
            GNAThub.EXEC_FAILURE: error happened while parsing the xml report

        """

        self.info('analyse report')

        tool = GNAThub.Tool(self.name)
        self.log.debug('parse XML report: %s', self.report)

        try:
            tree = ElementTree.parse(self.report)

            # Fetch all files
            files = tree.findall('./file')
            total = len(files)

            rules = {}
            # Dict of rules
            # key: name,  value: rule

            messages = {}
            # Dict of messages.
            # key: (rule, metric text), value: Message instance

            message_data = []
            # A list of message data suitable for bulk addition

            for index, node in enumerate(files, start=1):
                resource = GNAThub.Resource.get(node.attrib.get('name'))

                # Save file level metrics
                if not resource:
                    self.warn('skip "%s" message (file not found)' %
                              node.attrib.get('name'))
                    continue

                for metric in node.findall('./metric'):
                    name = metric.attrib.get('name')

                    if name in rules:
                        rule = rules[name]
                    else:
                        rule = GNAThub.Rule(name, name, GNAThub.METRIC_KIND,
                                            tool)
                        rules[name] = rule

                    if (rule, metric.text) in messages:
                        m = messages[(rule, metric.text)]
                    else:
                        m = GNAThub.Message(rule, metric.text)
                        messages[(rule, metric.text)] = m

                    message_data.append([m, 0, 1, 1])

                # Save unit level metric
                for unit in node.findall('.//unit'):
                    for metric in unit.findall('./metric'):
                        pass
                        # /!\ Not handled for now: to be done /!\
                        # File --> node.attrib.get('name'),
                        # Entity Line --> unit.attrib.get('line'),
                        # Entity name --> unit.attrib.get('name'),
                        # Entity Col --> unit.attrib.get('col'),
                        # Metric name --> metric.attrib.get('name'),
                        # Metric value --> metric.text)

                resource.add_messages(message_data)
                Console.progress(index, total, new_line=(index == total))

        except ParseError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to parse GNATcheck XML report')
            self.error('%s (%s:%s)' % (why, why.filename, why.lineno))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
