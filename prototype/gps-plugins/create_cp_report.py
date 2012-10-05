#! /usr/bin/env python

from utils import (Violation,
                   SourceMap,
                   Report,
                   ReportExporter)
import argparse
import os

REPORT='codepeer-report.json'
PRIORITIES = {'high' : 'CRITICAL',
              'medium' : 'MAJOR',
              'low' : 'MINOR',
              'informational' : 'INFO',
              'warning' : 'INFO',
              'check' : 'INFO'}

class CPOutputParser(object):

    def parse_output(self, report, output_path, src_map):
        with open(output_path, 'r') as cp_output:

            for line in cp_output.readlines():
                #Set maxsplit at 4 because the rule message can contain colon
                line_splited = line.split(':', 4)
                source = src_map.get_source(line_splited[0])
                if source:
                    prj = source.get_project()
                    directory = source.get_directory()
                    line = line_splited[1]
                    info_rule = line_splited[3].split(')')
                    rule_key = info_rule[0].strip()[1:]
                    priority = PRIORITIES[info_rule[1].strip()]
                    #In Sonar Codepeer rule repository, a rule has been
                    #duplicated for each priorities. See TN L919-022
                    rule_key += ' - ' + priority
                    msg = line_splited[4].strip()

                    violation = Violation(prj, directory, source.full_name, line,
                                          rule_key, msg)
                    report.add_violation(violation)

def _parse_command_line():
    parser = argparse.ArgumentParser(description='Codepeer report generator')
    parser.add_argument('--report-dir', action='store', dest='report_path',
                        type=str,
                        help='Absolute path to GNAT tools report directory',
                        required=True)

    parser.add_argument('--json-tree=', action='store', dest='json_tree', type=str,
                        help='Absolute path to json file that contains project tree',
                        required=True)
    parser.add_argument('--output=', action='store', dest='cp_output', type=str,
                       help='Absolute path to json file that contains Codepeer output')
    return parser.parse_args()

def _entry_point():
    """Script entry point"""
    cmd_line = _parse_command_line()
    codepeer_report = Report()
    cp_parser = CPOutputParser()
    src_map = SourceMap(cmd_line.json_tree)
    report_exporter = ReportExporter()

    cp_parser.parse_output(codepeer_report, cmd_line.cp_output, src_map)
    report_exporter.export_report(os.path.join(cmd_line.report_path, REPORT), codepeer_report)


if __name__ == '__main__':
    _entry_point()
