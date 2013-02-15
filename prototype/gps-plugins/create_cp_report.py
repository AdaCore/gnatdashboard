#! /usr/bin/env python

## Imports #################################################################
##
import argparse
import os
from utils import (Violation,
                   SourceMap,
                   Report,
                   ReportExporter)

RULE_SEPARATOR = '__'
REPORT='codepeer-report.json'
SEVERITIES = {'high' : 'CRITICAL',
              'medium' : 'MAJOR',
              'low' : 'MINOR',
              'informational' : 'INFO'}

##  CPOutputParser #############################################################
##
class CPOutputParser(object):
    """Allows to parse Codepeer output via the parse_output method.

       Parsed output is the result of the execution of:
       $ codepeer_msg_reader -csv path/project.output
    """

    def parse_output(self, report, output_path, src_map):
        """Parse Codepeer output.

          Parameters:
           - report: Report object where to save violations
           - output_path: path to the directory where the json report will be dumped
           - src_map: SourceMap object created from json tree file of the concerned
                      project. Allows to retrieve full path of a source file from its
                      basename.
        """
        try:
            with open(output_path, 'r') as cp_output:

                for line in cp_output.readlines()[1:]:
                    #Set maxsplit at 4 because the rule message can contain commas
                    line_splited = line.split(',', 8)
                    source_path = line_splited[0].split('/')
                    # Retrieves base name only

                    try:
                        source = src_map.get_source(source_path[len(source_path) - 1])

                        # Parsing source file information
                        prj = source.get_project()
                        directory = source.get_directory()
                        line = line_splited[1]

                        # Parsing rule's information
                        rule_key = line_splited[3]
                        severity = SEVERITIES[line_splited[6]]
                        category = line_splited[7]
                        # Removes double quote
                        message = line_splited[8][1:-1]

                        # In Sonar Codepeer rule repository, a rule has been
                        # duplicated for each priorities and each category.
                        # See TN L919-022
                        rule_key = severity.upper() + RULE_SEPARATOR  + category.upper() + RULE_SEPARATOR + rule_key

                        # Creates and saves the violation in given report
                        violation = Violation(prj, directory, source.full_name, line,
                                            rule_key, message)
                        report.add_violation(violation)
                    except KeyError as e:
                        print "Skipping message, because source not found in project tree: " + line_splited[0]
        except IOError as e:
            print e

## _parse_command_line  #############################################################
##
def _parse_command_line():
    """ Retrieves command line arguments
    """
    parser = argparse.ArgumentParser(description='Codepeer report generator')
    parser.add_argument('--report-dir', action='store', dest='report_path',
                        type=str,
                        help='Absolute path to GNAT tools report directory',
                        required=True)
    parser.add_argument('--json-tree=', action='store', dest='json_tree', type=str,
                        help='Absolute path to json file that contains project tree',
                        required=True)
    parser.add_argument('--output=', action='store', dest='cp_output', type=str,
                       help='Absolute path to the file that contains Codepeer output')
    return parser.parse_args()

## _entry_point #############################################################
##
def _entry_point():
    """Script entry point"""
    # Initialisation
    cmd_line = _parse_command_line()
    codepeer_report = Report()
    cp_parser = CPOutputParser()
    src_map = SourceMap(cmd_line.json_tree)
    report_exporter = ReportExporter()

    # Execution
    cp_parser.parse_output(codepeer_report, cmd_line.cp_output, src_map)
    report_exporter.export_report(os.path.join(cmd_line.report_path, REPORT), codepeer_report)


## Script Entry Point ######################################################
##
if __name__ == '__main__':
    _entry_point()

