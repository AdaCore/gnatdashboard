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

"""GNAThub plug-in for the GNATmetric and LALmetric command-line tool

It exports the GNATmetric class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import os

import GNAThub
from GNAThub import Console, Plugin, Reporter, Runner

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError


class GNATmetric(Plugin, Runner, Reporter):
    """GNATmetric & LALmetric plugin for GNAThub"""

    # GNATmetric exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        super(GNATmetric, self).__init__()

        self.tool = None
        self.output = os.path.join(GNAThub.Project.object_dir(), 'metrix.xml')

    @property
    def name(self):
        return 'lalmetric' if self._use_libadalang_tools else 'gnatmetric'

    @property
    def _use_libadalang_tools(self):
        """Whether to use GNATmetric or LALmetric

        :return: `True` if we should use LALmetric, `False` for GNATmetric
        :rtype: boolean
        """
        return 'USE_LIBADALANG_TOOLS' in os.environ

    def __cmd_line(self):
        """Creates GNATmetric command line arguments list

        :return: the GNATmetric command line
        :rtype: list[str]
        """

        cmd_line = [
            self.name, '-ox', self.output, '-P', GNAThub.Project.path(), '-U'
        ] + GNAThub.Project.scenario_switches()
        if GNAThub.Project.target():
            cmd_line[0] = '{}-{}'.format(GNAThub.Project.target(), cmd_line[0])
        if GNAThub.Project.runtime():
            cmd_line.extend(('--RTS', GNAThub.Project.runtime()))
        return cmd_line

    def run(self):
        """Executes GNATmetric

        Returns according to the success of the execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status in GNATmetric.VALID_EXIT_CODES else GNAThub.EXEC_FAILURE

    def report(self):
        """Parses GNATmetric XML report and save data to the database

        Returns according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: transactions committed to database
            * ``GNAThub.EXEC_FAILURE``: error while parsing the xml report
        """

        self.info('clear existing results if any')
        GNAThub.Tool.clear_references(self.name)

        self.info('analyse report')

        tool = GNAThub.Tool(self.name)
        self.log.debug('parse XML report: %s', self.output)

        try:
            tree = ElementTree.parse(self.output)

            # Fetch all files
            files = tree.findall('./file')
            total = len(files)

            # Map of rules (couple (name, rule): dict[str,Rule])
            rules = {}

            # Map of messages (couple (rule, message): dict[str,Message])
            messages = {}

            for index, node in enumerate(files, start=1):
                resource = GNAThub.Resource.get(node.attrib.get('name'))

                # A list of message data suitable for bulk addition
                message_data = []

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
                        rule = GNAThub.Rule(
                            name, name, GNAThub.METRIC_KIND, tool)
                        rules[name] = rule

                    if (rule, metric.text) in messages:
                        msg = messages[(rule, metric.text)]
                    else:
                        msg = GNAThub.Message(rule, metric.text)
                        messages[(rule, metric.text)] = msg

                    message_data.append([msg, 0, 1, 1])

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
            self.log.exception('failed to parse XML report')
            self.error('%s (%s:%s)' % (why, why.filename, why.lineno))
            return GNAThub.EXEC_FAILURE

        else:
            return GNAThub.EXEC_SUCCESS
