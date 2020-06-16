# GNAThub (GNATdashboard)
# Copyright (C) 2013-2020, AdaCore
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

"""GNAThub plug-in for the GNATstack command-line tool.

It exports the GNATstack class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import os

import GNAThub
from GNAThub import Plugin, Reporter, Runner

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError


class GNATstack(Plugin, Runner, Reporter):
    """GNATstack plugin for GNAThub."""

    # GNATstack exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)
    RANKING = GNAThub.RANKING_INFO

    def __init__(self):
        super(GNATstack, self).__init__()

        if GNAThub.dry_run_without_project():
            return

        self.tool = None
        # FIXME: For now we can't control where the xml file is created:
        # it is always created in the project directory
        self.output = os.path.join(os.path.dirname(GNAThub.Project.path()),
                                   "stack_usage.xml")
        self.resources = {}

        # Map of ID => entity
        self.subprograms = {}

        # Map of ID => name
        self.subprograms_without_location = {}

    def __cmd_line(self):
        """Create GNATstack command line arguments list.

        :return: the GNATstack command line
        :rtype: collections.Iterable[str]
        """

        cmd_line = [
            'gnatstack', '-Q', '-x', '-Wa',
            '-P', GNAThub.Project.path()] + GNAThub.Project.scenario_switches()
        if GNAThub.Project.runtime():
            cmd_line.extend(('--RTS', GNAThub.Project.runtime()))
        if GNAThub.subdirs():
            cmd_line.extend(['--subdirs=' + GNAThub.subdirs()])
        return cmd_line

    def run(self):
        """Execute GNATstack.

        Returns according to the success of the execution of the tool:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution
            * ``GNAThub.EXEC_FAILURE``: on any error
        """
        return GNAThub.EXEC_SUCCESS if GNAThub.Run(
            self.name, self.__cmd_line()
        ).status in GNATstack.VALID_EXIT_CODES else GNAThub.EXEC_FAILURE

    def pp_name(self, name):
        if '<' in name:
            return name
        else:
            return name.split('.')[-1].title()

    def pp_msg(self, id, msg):
        return "[{}] {}".format(msg, self.subprograms[id].name)

    def report(self):
        """Parse GNATstack output file report.

        Returns according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error
        """
        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.log.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        self.info('analyse report')

        self.tool = GNAThub.Tool(self.name)
        if not os.path.exists(self.output):
            self.error('no report found')
            return GNAThub.EXEC_FAILURE
        self.log.debug('parse XML report: %s', self.output)

        # List of resource messages suitable for tool level bulk insertion
        resources_messages = []
        # Map of list of messages by entities
        entities_messages_map = {}
        # List of entity messages suitable for tool level bulk insertion
        entities_messages = []
        # List of indirect call location
        indirect_loc_list = []

        try:
            tree = ElementTree.parse(self.output)
            global_node = tree.find('./global')

            # Retrieve the metrics and the map of subprogram by id
            subprograms = tree.find('./subprogramset').findall('./subprogram')
            if subprograms:
                unknown_global = GNAThub.Rule("Unknown Global Stack Usage",
                                              "Unknown Global Stack Usage",
                                              GNAThub.METRIC_KIND,
                                              self.tool)
                static_global = GNAThub.Rule("Static Global Stack Usage",
                                             "Static Global Stack Usage",
                                             GNAThub.METRIC_KIND,
                                             self.tool)
                unknown_local = GNAThub.Rule("Unknown Local Stack Usage",
                                             "Unknown Local Stack Usage",
                                             GNAThub.METRIC_KIND,
                                             self.tool)
                static_local = GNAThub.Rule("Static Local Stack Usage",
                                            "Static Local Stack Usage",
                                            GNAThub.METRIC_KIND,
                                            self.tool)
            for node in subprograms:
                subprogram_id = node.attrib.get('id')
                locations = node.find('./locationset').findall('./location')
                global_usage = node.find('./globalstackusage')
                local_usage = node.find('./localstackusage')
                name = node.attrib.get('prefixname')
                if name == "indirect call":
                    # The columns are only defined here so save them for later
                    line = locations[0].attrib.get('line')
                    column = locations[0].attrib.get('column')
                    indirect_loc_list.append([line, column])
                    continue
                else:
                    name = self.pp_name(name)

                if not locations:
                    if subprogram_id not in self.subprograms_without_location:
                        self.subprograms_without_location[subprogram_id] = name

                for loc in locations:
                    file = loc.attrib.get('file')
                    line = loc.attrib.get('line')
                    column = loc.attrib.get('column')
                    if file in self.resources:
                        resource = self.resources[file]
                    else:
                        resource = GNAThub.Resource(file, GNAThub.FILE_KIND)
                        self.resources[file] = resource

                    # entities default value for kind is set to "procedure"
                    entity = GNAThub.Entity(name, "action",
                                            int(line), int(column),
                                            int(column), resource)
                    # Only link the id to the first location of the entity
                    if subprogram_id not in self.subprograms:
                        self.subprograms[subprogram_id] = entity
                    else:
                        continue

                    size = global_usage.attrib.get('size')
                    if global_usage.attrib.get('qualifier') == "UNKNOWN":
                        metric = unknown_global
                    else:
                        metric = static_global
                    global_metric = GNAThub.Message(metric,
                                                    size,
                                                    ranking=GNATstack.RANKING)

                    size = local_usage.attrib.get('size')
                    if local_usage.attrib.get('qualifier') == "UNKNOWN":
                        metric = unknown_local
                    else:
                        metric = static_local
                    local_metric = GNAThub.Message(metric,
                                                   size,
                                                   ranking=GNATstack.RANKING)

                    entities_messages_map[subprogram_id] = (
                        [[global_metric, 0, 1, 1], [local_metric, 0, 1, 1]])

            # Analyse the indirect calls
            indirects = global_node.find('./indirectset').findall('./indirect')
            if indirects:
                indirect_rule = GNAThub.Rule("Indirect Call",
                                             "Indirect Call",
                                             GNAThub.RULE_KIND,
                                             self.tool)
            for node in indirects:
                indirect_id = node.attrib.get('id')
                if indirect_id not in self.subprograms:
                    continue

                set = node.find('./indirectcallset').findall('./indirectcall')
                for call in set:
                    line = call.find('./line').find('./value').text
                    # Go through the list of saved locations and use the
                    # line to retrieve a corresponding column
                    column = 1
                    pos = -1
                    for ix in range(len(indirect_loc_list)):
                        if indirect_loc_list[ix][0] == line:
                            pos = ix
                            column = indirect_loc_list[pos][1]
                            continue
                    if pos != -1:
                        indirect_loc_list.pop(pos)
                    message = GNAThub.Message(indirect_rule,
                                              self.pp_msg(indirect_id,
                                                          "indirect call"),
                                              ranking=GNATstack.RANKING)
                    entities_messages_map[indirect_id].append([message,
                                                               int(line),
                                                               int(column),
                                                               int(column)])

            # Analyse the external calls
            externals = global_node.find('./externalset').findall('./external')
            if externals:
                external_rule = GNAThub.Rule("External Call",
                                             "External Call",
                                             GNAThub.RULE_KIND,
                                             self.tool)
            for node in externals:
                subprogram_id = node.attrib.get('id')
                if subprogram_id not in self.subprograms:
                    continue
                message = GNAThub.Message(external_rule,
                                          self.pp_msg(subprogram_id,
                                                      "external call"),
                                          ranking=GNATstack.RANKING)
                entities_messages_map[subprogram_id].append([message, 0, 1, 1])

            # Analyse the potential cycle
            cycles = global_node.find('./cycleset').findall('./cycle')
            if cycles:
                cycle_rule = GNAThub.Rule("Potential Cycle",
                                          "Potential Cycle",
                                          GNAThub.RULE_KIND,
                                          self.tool)
            for node in cycles:
                cycle_subprograms = node.findall('./subprogram')
                cycle_list = []
                for sub in cycle_subprograms:
                    subprogram_id = sub.attrib.get('id')
                    cycle_list.append(self.subprograms[subprogram_id].name)
                subprogram_id = cycle_subprograms[0].attrib.get('id')
                cycle_list.append(self.subprograms[subprogram_id].name)
                message = GNAThub.Message(cycle_rule,
                                          "potential cycle detected:\n\t\t" +
                                          "\n\t\t".join(cycle_list),
                                          ranking=GNATstack.RANKING)
                entities_messages_map[subprogram_id].append([message, 0, 1, 1])

            # Analyse the unbounded frames
            unboundeds = (
                global_node.find('./unboundedset').findall('./unbounded'))
            if unboundeds:
                unbounded_rule = GNAThub.Rule("Unbounded Frame",
                                              "Unbounded Frame",
                                              GNAThub.RULE_KIND,
                                              self.tool)
            for node in unboundeds:
                subprogram_id = node.attrib.get('id')
                if subprogram_id in self.subprograms:
                    message = GNAThub.Message(unbounded_rule,
                                              self.pp_msg(subprogram_id,
                                                          "unbounded frame"),
                                              ranking=GNATstack.RANKING)
                    entities_messages_map[subprogram_id].append([message,
                                                                 0, 1, 1])

            # Analyse the entry points
            entries = tree.find('./entryset').findall('./entry')
            # There is always an entry, so create the rule anyway
            entry_rule = GNAThub.Rule("Entry point",
                                      "Entry point",
                                      GNAThub.RULE_KIND,
                                      self.tool)
            for node in entries:
                subprogram_id = node.attrib.get('id')

                if subprogram_id not in self.subprograms:
                    continue
                entity = self.subprograms[subprogram_id]
                local_stack = node.find('./localstackusage')
                size = local_stack.attrib.get('size')
                qualifier = local_stack.attrib.get('qualifier')

                if qualifier == "UNKNOWN":
                    text = "The estimated"
                else:
                    text = "The"
                text += (' call stack size for the entry point "%s" is %s' %
                         (entity.name, str(size)))

                callchain_list = []
                for sub in node.find('./callchain').findall('./subprogram'):
                    chain_id = sub.attrib.get('id')
                    if chain_id in self.subprograms:
                        callchain_list.append(self.subprograms[chain_id].name)
                    elif chain_id in self.subprograms_without_location:
                        callchain_list.append(
                            self.subprograms_without_location[chain_id])
                    else:
                        continue

                text += (" and the callchain is:\n\t\t%s" %
                         "\n\t\t".join(callchain_list))
                message = GNAThub.Message(entry_rule,
                                          text,
                                          ranking=GNATstack.RANKING)
                entities_messages_map[subprogram_id].append([message, 0, 1, 1])

            # Project message explaining the accuracy of the metrics
            accurate = global_node.find('./accurate')
            if accurate.find('./value').text == "FALSE":
                project = GNAThub.Resource(GNAThub.Project.name(),
                                           GNAThub.PROJECT_KIND)
                rule = GNAThub.Rule("Accuracy", "Accuracy",
                                    GNAThub.RULE_KIND, self.tool)
                text = ("worst case may not be accurate because of: " +
                        ("indirect calls/" if indirects else "") +
                        ("cycles/" if cycles else "") +
                        ("unbounded frames/" if unboundeds else "") +
                        ("external calls" if externals else ""))
                text = text[:-1] if text[-1] == '/' else text
                message = GNAThub.Message(rule,
                                          text,
                                          ranking=GNATstack.RANKING)
                resources_messages.append([project, [[message, 0, 1, 1]]])

            # Insert the messages and the metrics
            for key, value in entities_messages_map.iteritems():
                entities_messages.append([self.subprograms[key], value])
            self.tool.add_messages(resources_messages, entities_messages)
        except ParseError as why:
            self.log.exception('failed to parse XML report')
            self.error('%s (%s:%s)' % (why, why.filename, why.lineno))
            return GNAThub.EXEC_FAILURE
        else:
            return GNAThub.EXEC_SUCCESS
