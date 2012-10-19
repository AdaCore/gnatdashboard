#! /usr/bin/env python

from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element
from xml.etree.ElementTree import SubElement
from xml.etree.ElementTree import Comment
from xml.etree.ElementTree import tostring
import argparse
import os

#############
# CONSTANTS #
#############
FILE_METRICS = ['all_lines', 'code_lines', 'comment_lines',
                'eol_comments', 'comment_percentage', 'blank_lines']

PACKAGE_METRICS = ['unit_nesting', 'construct_nesting', 'all_stmts', 'all_dcls',
                   'lsloc', 'all_subprograms', 'all_types', 'unit_nesting',
                   'private_types', 'tagged_types', 'abstract_types',
                   'public_types', 'public_subprograms']

UNIT_AVG_METRICS = ['statement_complexity', 'short_circuit_complexity',
                    'cyclomatic_complexity', 'essential_complexity']

UNIT_MAX_METRIC = ['max_loop_nesting']
UNIT_ADD_METRICS = ['extra_exit_points',]


## GmOutput #################################################################
##

class GmOutput(object):
    def __init__(self, gm_output):
      self.gm_output = gm_output
      self.metric_by_file = dict()

    def _init_dict(self):
        metrics = dict()

        all_metrics = PACKAGE_METRICS + UNIT_ADD_METRICS +UNIT_AVG_METRICS + UNIT_MAX_METRIC
        for metric in all_metrics:
          metrics[metric] = 0

        return metrics

    def parse_output(self):
        print 'Parsing GNAT Metric output...'

        with open(self.gm_output) as output:
            tree = ElementTree.parse(output)

        for file_node in tree.findall('./file'):

            # Initializes dictionnary of metric for each file
            # and counter to compute average.
            metrics = self._init_dict()
            counter = 0

            # Retrieves metrics at file level
            for metric in file_node.findall('./metric'):
                if metric.attrib.get('name') == 'comment_percentage':
                    metrics[metric.attrib.get('name')] = float(metric.text)
                else:
                    metrics[metric.attrib.get('name')] = int(metric.text)

            # Retrieves metrics at unit level
            for unit in file_node.findall('.//unit'):

                # Count the number of unit: function or procedure in the file for
                # average
                if not unit.attrib.get('kind').startswith('package'):
                    counter +=1

                for m in unit.findall('./metric'):
                    key = m.attrib.get('name')

                    if key not in FILE_METRICS:
                        # Skip metric that are duplicated in function/method unit
                        # duplicated from package unit
                        if not unit.attrib.get('kind').startswith('package') and key in PACKAGE_METRICS:
                            continue

                        #Retrieves max for max_loop_nesting
                        if key == UNIT_MAX_METRIC[0]:
                            if metrics[key] < int(m.text): metrics[key] = int(m.text)
                        else:
                            metrics[key] += int(m.text)

            # Saves dictionnary of metrics for the current file after
            # average computing.
            self._compute_avg(metrics, counter)
            self.metric_by_file[file_node.attrib.get('name')] = metrics


    def _compute_avg(self, metrics, counter):
        for m in UNIT_AVG_METRICS:
            if counter:
                metrics[m] = float(metrics[m])
                metrics[m] /= counter

## ReportExporter #################################################################
##

class ReportExporter(object):

    REPORT_NAME = 'gnatmetric-report.xml'

    def __init__(self, report_path):
        self.report_path = os.path.join(report_path, self.REPORT_NAME)

    def prettify(self, elem):
        """Return a pretty-printed XML string for the Element"""
        rough_string = ElementTree.tostring(elem, 'utf-8')
        reparsed = minidom.parseString(rough_string)
        return reparsed.toprettyxml(indent="  ")


    def export(self, gm_output):
        global_xml = Element('global')

        comment = Comment('GNAT Metric results')
        global_xml.append(comment)

        for fil in gm_output.metric_by_file:
            file_xml = SubElement(global_xml, 'file', {'name':fil})

            for metric in gm_output.metric_by_file[fil]:
                metric_xml = SubElement(file_xml, 'metric', {'name':metric})
                metric_xml.text = str(gm_output.metric_by_file[fil][metric])

        pretty_xml = self.prettify(global_xml)
        with open(self.report_path, 'w+') as gm_report:
            print 'Creating GNAT Metric XML report...'
            gm_report.writelines(pretty_xml)


## _parse_command_line ###########################################################
##

def _parse_command_line():
    """Parse the command line"""
    parser = argparse.ArgumentParser(description=
                                     'Gnat Check report generator')
    parser.add_argument('--target=', action="store", dest="target",
                        type=str, help="Absolute path target for the generated GNAT Metric report",
                        required=True)
    parser.add_argument('--gm_output=', action="store", dest="gm_output",
                        type=str, help="Absolute path to metrix.xml file", required=True)
    parser.add_argument('--json-tree=', action='store', dest='json_tree', type=str,
                        help='Absolute path to json file that contains project tree',
                        required=True)
    return parser.parse_args()


## _entry_point ###########################################################
##

def _entry_point():
    """Script_entry point"""
    cmd_line = _parse_command_line()
    gm_output = GmOutput(cmd_line.gm_output)
    gm_output.parse_output()
    exporter = ReportExporter(cmd_line.target)
    exporter.export(gm_output)

## Script Entry Point ######################################################
##

if __name__ == '__main__':
    """Calls the entry point and uses its return value as exit code."""
    _entry_point()
