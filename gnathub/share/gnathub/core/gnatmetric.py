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

"""GNAThub plug-in for the GNATmetric and LALmetric command-line tool.

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
    """GNATmetric & LALmetric plugin for GNAThub."""

    # GNATmetric exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)
    RANKING = GNAThub.RANKING_INFO

    def __init__(self):
        super(GNATmetric, self).__init__()

        self.tool = None
        self.output = os.path.join(GNAThub.Project.object_dir(), 'metrix.xml')
        self.rules = {}
        self.messages = {}
        self.firstunit = False

    @property
    def name(self):
        return 'lalmetric' if self._use_libadalang_tools else 'gnatmetric'

    @property
    def _use_libadalang_tools(self):
        """Whether to use GNATmetric or LALmetric.

        :return: `True` if we should use LALmetric, `False` for GNATmetric
        :rtype: boolean
        """
        return 'USE_LIBADALANG_TOOLS' in os.environ

    def __cmd_line(self):
        """Create GNATmetric command line arguments list.

        :return: the GNATmetric command line
        :rtype: collections.Iterable[str]
        """
        cmd_line = [
            self.name, '-ox', self.output, '-P', GNAThub.Project.path()]

        if GNAThub.u_process_all():
            cmd_line.extend(['-U'])

#  Keeping this for later implementation of -U main switch
#        if GNAThub.u_main():
#            cmd_line.extend(['-U'])
#            cmd_line.extend([GNAThub.u_main()])

        cmd_line = cmd_line + GNAThub.Project.scenario_switches()

        if GNAThub.Project.target():
            cmd_line[0] = '{}-{}'.format(GNAThub.Project.target(), cmd_line[0])
        if GNAThub.Project.runtime():
            cmd_line.extend(('--RTS', GNAThub.Project.runtime()))
        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])

        return cmd_line

    def run(self):
        """Execute GNATmetric.

        Returns according to the success of the execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """

        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status in GNATmetric.VALID_EXIT_CODES else GNAThub.EXEC_FAILURE

    def parse_metrics(self, node, entity=False):
        """Parse the xml *node* returns a list of metrics"""
        message_data = []

        for metric in node.findall('./metric'):
            name = metric.attrib.get('name')

            if name in self.rules:
                rule = self.rules[name]
            else:
                rule = GNAThub.Rule(
                    name, name, GNAThub.METRIC_KIND, self.tool)
                self.rules[name] = rule

            if (rule, metric.text, GNATmetric.RANKING) in self.messages:
                msg = self.messages[(rule, metric.text, GNATmetric.RANKING)]
            else:
                msg = GNAThub.Message(rule, metric.text, GNATmetric.RANKING)
                self.messages[(rule, metric.text, GNATmetric.RANKING)] = msg

            message_data.append([msg, 0, 1, 1])
        return message_data

    def parse_units(self, node, resource):
        """Recursively parse the unit node until all of them are found"""
        # Map of entities for a ressource
        entities_messages = []

        if not node.findall('./unit'):
            return []

        for unit in node.findall('./unit'):

            ename = unit.attrib.get('name')

            ekind = unit.attrib.get('kind')
            if self.firstunit:
                ekind = "compilation unit"
                self.firstunit = False
            else:
                if ekind.startswith('procedure'):
                    ekind = ekind.replace("procedure", "action")
                elif ekind.startswith('function'):
                    ekind = ekind.replace("function", "action")

            eline = unit.attrib.get('line')
            ecol = unit.attrib.get('col')

            # A resource can have multiple entities with the same name
            entity = GNAThub.Entity(ename, ekind, int(eline),
                                    int(ecol), int(ecol),
                                    resource)

            entities_messages.append([entity, self.parse_metrics(unit, True)])
            entities_messages += self.parse_units(unit, resource)
        return entities_messages

    def parse_config(self, tree):
        """Parse the config block to create the GNAThub rules, if any"""

        config_node = tree.find('./config')

        if config_node is not None:
            return

        self.info('retrieving metrics configuration')

        for metric in config_node.findall('./metric'):
            name = metric.attrib.get('name')
            display_name = metric.attrib.get('display_name')

            if not display_name:
                display_name = name

            if name in self.rules:
                rule = self.rules[name]
            else:
                rule = GNAThub.Rule(
                    display_name, name, GNAThub.METRIC_KIND, self.tool)
                self.rules[name] = rule

    def report(self):
        """Parse GNATmetric XML report and save data to the database.

        Returns according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: transactions committed to database
            * ``GNAThub.EXEC_FAILURE``: error while parsing the xml report
        """

        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        self.info('analyse report')

        self.tool = GNAThub.Tool(self.name)
        self.log.debug('parse XML report: %s', self.output)

        try:
            tree = ElementTree.parse(self.output)

            # Parse the config first to create the GNAThub rules
            self.parse_config(tree)

            # Fetch all files
            files = tree.findall('./file')
            total = len(files)

            # List of resource messages suitable for tool level bulk insertion
            resources_messages = []

            for index, node in enumerate(files, start=1):
                resource = GNAThub.Resource.get(node.attrib.get('name'))

                # Save file level metrics
                if not resource:
                    self.warn('skip "%s" message (file not found)' %
                              node.attrib.get('name'))
                    continue

                self.firstunit = True

                resources_messages.append([resource, self.parse_metrics(node)])

                self.tool.add_messages([], self.parse_units(node, resource))

                Console.progress(index, total, new_line=(index == total))

            # Retrieve the project metrics
            resource = GNAThub.Resource(GNAThub.Project.name(),
                                        GNAThub.PROJECT_KIND)
            resources_messages.append([resource, self.parse_metrics(tree)])
            self.tool.add_messages(resources_messages, [])

        except ParseError as why:
            self.log.exception('failed to parse XML report')
            self.error('%s (%s:%s)' % (why, why.filename, why.lineno))
            return GNAThub.EXEC_FAILURE

        else:
            return GNAThub.EXEC_SUCCESS
