#! /usr/bin/env python

## Imports #################################################################
##
import argparse
import os
from utils import (Violation,
                   SourceMap,
                   Report,
                   ReportExporter)

REPORT = 'gcov-report.json'


## Hit #############################################################
##
class Hit (object):
    """ Represents number of hits for a line.

      /!\ Not use for now but may be for non prototype version.
    """

    def __init__(self, line, number):
        self.line = line
        self.number = number

    def reprJSON(self):
        return dict(line=self.line, hits=self.number)

## GCovOutput #############################################################
##
class GCovOutput (object):
    """Represents Gcov output that will be dumped in json format """

    def __init__(self, src, directory, project):
        self.src = src
        self.directory = directory
        self.project = project
        # Key=line, value=number of hits
        self.hits = dict()

    def add_hit_for_line(self, line, hits):
        self.hits[line] = hits

    def reprJSON(self):
        return dict(src=self.src, directory=self.directory, prj=self.project,
                    hits=self.hits)

## GCovOutputParser ###########################################################
##
class GCovOutputParser (object):
    """Allows to parse gcov output via the parse_output method.

      Browse all sources retrieved from project file and look for corresponding
      gcov generated file.
    """

    def get_gcov_location(self, src_map, basename, gcov_path):
        """Retrieves gcov file location from basename source.

          Return: None if gcov file not found
          /!\ Prototype version: to be re-arranged accordingly to real
              connstraints. For now, returns the same location, for test.
        """
        if gcov_path != '':
            return os.path.join(gcov_path, basename + '.gcov')
        elif src_map.get_obj_dir(basename):
            return os.path.join(src_map.get_obj_dir(basename),
                                basename + '.gcov')
        else:
            return None

    def parse_output(self, gcov_report, src_map, gcov_path):
        """Parse gcov output.

          Parameters:
           - src_map: SourceMap object created from json tree file of the
                      concerned project. Allows to retrieve full path of a
                      source file from its basename.
        """
        sources = [basename for basename in src_map.get_all_basename()
                   if '.adb' in basename]
        for basename in sources:
            gcov_location = self.get_gcov_location(src_map, basename, gcov_path)
            # Create a report for the project

            # If gcov file has been found
            if gcov_location:
                try:
                    with open(gcov_location, 'r') as gcov_file:

                        # Create GCoveOutput for the source
                        gcov_output = GCovOutput(src_map.get_path(basename),
                                                 src_map.get_directory(basename),
                                                 src_map.get_project(basename))

                        # Retrieve information for every source line
                        # skip first 2 lines
                        for line in gcov_file.readlines()[2:]:
                            # Skip useless line
                            if line.strip()[0] != '-':
                                line_infos = line.split(':', 2)
                                hits = line_infos[0].strip()
                                # Line is not covered
                                if hits == '#####' or hits == '=====':
                                    hits = '0'
                                line_id = line_infos[1].strip()
                                gcov_output.add_hit_for_line(line_id, hits)

                    #Save GCovOutput for the source in report
                    # 'add_violations': function's name doesn't fit here,
                    #  will be changed for non prototype version.
                    gcov_report.add_violation(gcov_output)
                except IOError as e:
                    print 'Unable to process gcov file for source : ' + basename
                    print e

            else:
                print 'Unable to found gcov file for source: ' + basename
                print 'Location for gcov output files can be set through switch : --gcov-path=/absolute/path/to/location'

## _parse_command_line  #######################################################
##
def _parse_command_line():
    """ Retrieves command line arguments
    """
    parser = argparse.ArgumentParser(description='Codepeer report generator')
    parser.add_argument('--report-dir', action='store', dest='report_path',
                        type=str,
                        help='Absolute path to GNAT tools report directory',
                        required=True)
    parser.add_argument('--json-tree=', action='store', dest='json_tree',
                        type=str, help='Absolute path to json file that' +
                        'contains project tree', required=True)
    parser.add_argument('--gcov-path=', action='store', dest='gcov_path',
                        type=str, help='Absolute path to .gcov file',
                        default=None, required=False)
    return parser.parse_args()

## _entry_point #############################################################
##
def _entry_point():
    """Script entry point"""
    # Initialisation
    cmd_line = _parse_command_line()
    gcov_report = Report()
    gcov_parser = GCovOutputParser()
    src_map = SourceMap(cmd_line.json_tree)
    report_exporter = ReportExporter()

    # Execution
    gcov_parser.parse_output(gcov_report, src_map, cmd_line.gcov_path)
    report_exporter.export_report(os.path.join(cmd_line.report_path, REPORT),
                                  gcov_report)


## Script Entry Point ######################################################
##
if __name__ == '__main__':
    _entry_point()
