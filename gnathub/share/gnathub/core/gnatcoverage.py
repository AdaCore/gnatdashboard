# GNAThub (GNATdashboard)
# Copyright (C) 2014-2017, AdaCore
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

"""GNAThub plug-in for the GNATcoverage command-line tool.

It exports the GNATcoverage class which implements the :class:`GNAThub.Plugin`
interface.  This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import os
from xml.dom import minidom

import GNAThub
from GNAThub import Console, Plugin, Reporter


class GNATcoverage(Plugin, Reporter):
    """GNATcoverage plugin for GNAThub.

    Retrieves .xcov generated files from the project root object directory,
    parses them and feeds the database with the data collected from each files.
    """
    GNATCOVERAGE_OUTPUT = os.path.join(GNAThub.Project.object_dir())
    XML_EXT = '.xml'

    def __init__(self):
        super(GNATcoverage, self).__init__()

        self.tool = None
        # Mapping: coverage level -> issue rule for this coverage.
        self.issue_rules = {}

    def __process_file(self, resource, filename,
                       resources_messages, file_metrics):
        """Processe one file, adding in bulk all coverage info found.

        :param GNAThub.Resource resource: the resource being processed
        :param str filename: the name of the resource
        """

        bulk_messages = []
        file_path = os.path.join(self.GNATCOVERAGE_OUTPUT,
                                 filename) + self.XML_EXT

        def add_message(rule, message, ranking, line_no, column_no):
            bulk_messages.append([
                GNAThub.Message(rule, message, ranking=ranking),
                line_no, column_no, column_no
            ])

        file_xml = minidom.parse(file_path)
        lines = file_xml.getElementsByTagName('src_mapping')

        # Get the informations needed to create messages
        for index, line in enumerate(lines, start=1):
            cov_rule = self.issue_rules['coverage']
            cov_char = line.attributes['coverage'].value
            cov_status = {
                    '.': 'NO_CODE',
                    '+': 'COVERED',
                    '-': 'NOT_COVERED',
                    '!': 'PARTIALLY_COVERED'
                 }[cov_char]
            line_info = line.getElementsByTagName('line')
            line_no = int(line_info[0].attributes['num'].value)
            if line_info.length > 1:
                column_no = int(line_info[1].
                                attributes['column_begin'].value)
            # Create the coverage per line message
            add_message(cov_rule, cov_status,
                        GNAThub.RANKING_UNSPECIFIED, line_no, 0)

            message_info = line.getElementsByTagName('message')
            if message_info.length > 0:
                sco = message_info[0].attributes['SCO'].value.split(' ')[2]
                cov_level = {
                    'STATEMENT': 'statement',
                    'DECISION':  'decision',
                    'CONDITION': 'condition',
                }[sco]
                cov_rule = self.issue_rules[cov_level]
                message_label = message_info[0].attributes['message'].value
                message_label = cov_level + " " + message_label
                # Create the message to show in interface
                add_message(cov_rule, message_label,
                            GNAThub.RANKING_UNSPECIFIED, line_no, column_no)

        # Create message for gnatcoverage metric
        metrics = {}
        for metric in file_metrics:
            name = str(metric['name'])
            value = str(metric['value'])
            ratio = str(metric['ratio'])

            if name in self.issue_rules:
                rule = self.issue_rules[name]
            else:
                rule = GNAThub.Rule(
                    name, name, GNAThub.METRIC_KIND, self.tool)
                self.issue_rules[name] = rule

            # Add message so it's registered like a GNATmetric informations
            add_message(rule, value, GNAThub.RANKING_UNSPECIFIED, 0, 0)

            # Format metrics, so it can be linked to source information
            # by _report.py
            metrics[name] = str({
                'value': value,
                'ratio': ratio
            })

        # Insert metrics in GNAThub DB
        gnatcov_metric_rule = GNAThub.Rule(
                    'gnatcov', 'gnatcov', GNAThub.METRIC_KIND, self.tool)
        self.issue_rules['gnatcov'] = rule
        add_message(gnatcov_metric_rule, str(metrics),
                    GNAThub.RANKING_UNSPECIFIED, 0, 0)

        # Preparing list for tool level insertion of resources messages
        resources_messages.append([resource, bulk_messages])

    def report(self):
        """Analyse the report files generated by :program:`GNATcoverage`.

        Finds all .xcov files in the object directory and parses them.

        Sets the exec_status property according to the success of the analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error
        """
        # Clear existing references only if not incremental run
        if not GNAThub.incremental():
            self.log.info('clear existing results if any')
            GNAThub.Tool.clear_references(self.name)

        # Check if gnatcoverage output folder exist
        if not os.path.exists(self.GNATCOVERAGE_OUTPUT):
            self.log.info('No gnatcoverage folder in object directory')
            return GNAThub.EXEC_FAILURE

        self.info('parse coverage reports (%s)' % self.XML_EXT)
        # Fetch all files in project object directory and retrieve only
        # .xml files, absolute path
        index_path = os.path.join(self.GNATCOVERAGE_OUTPUT, 'index.xml')
        index_xml = minidom.parse(index_path)
        if not index_xml:
            self.error('no %s file in object directory' % self.XML_EXT)
            return GNAThub.EXEC_FAILURE

        files = index_xml.getElementsByTagName('file')
        self.tool = GNAThub.Tool(self.name)
        for cov_level in ('statement', 'decision', 'condition', 'coverage'):
            self.issue_rules[cov_level] = GNAThub.Rule(
                cov_level, cov_level, GNAThub.RULE_KIND, self.tool)
        total = files.length

        # List of resource messages suitable for tool level bulk insertion
        resources_messages = []

        try:
            for index, file in enumerate(files, start=1):
                # Retrieve source fullname
                filename = file.attributes['name'].value
                print "Process ", filename

                metrics = file.getElementsByTagName('metric')
                file_metrics = []

                for metric in metrics:
                    myMetric = {
                        'name': str(metric.getAttribute('kind')),
                        'value': str(metric.getAttribute('count')),
                        'ratio': str(metric.getAttribute('ratio'))
                    }
                    file_metrics.append(myMetric)

                src = GNAThub.Project.source_file(filename)
                resource = GNAThub.Resource.get(src)
                if resource:
                    self.__process_file(resource, filename,
                                        resources_messages, file_metrics)

                Console.progress(index, total, new_line=(index == total))

            # Tool level insert for resources messages
            self.tool.add_messages(resources_messages, [])

        except (IOError, ValueError) as why:
            self.log.exception('failed to parse reports')
            self.error(str(why))
            return GNAThub.EXEC_FAILURE

        else:
            return GNAThub.EXEC_SUCCESS
