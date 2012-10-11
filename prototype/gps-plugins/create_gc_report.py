## Imports #################################################################
##
from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element
from xml.etree.ElementTree import SubElement
from xml.etree.ElementTree import Comment
from xml.etree.ElementTree import tostring
from subprocess import Popen
from subprocess import PIPE
from subprocess import STDOUT
from utils import (Project,
                   Directory,
                   Source,
                   SourceMap)
import re
import sys
import json
import argparse

## Violation #################################################################
##
class Violation(object):
    """Represent a rule violation in GnatCheck analyse"""
    def __init__(self, src, line, rule_id, msg):
        self.src = src
        self.line = line
        self.rule_id = rule_id
        self.msg = msg


# GcOutput #################################################################
##
class GcOutput(object):
    """Represent the ouput of GnatCheck

       Is initialized with the path to the GnatCheck log file which
       contains the output of GnatCheck, parse it and save all the
       rules violations.
    """
    def __init__(self, gc_log):
        """Initializes the class by processing the GnatCheck output

           and saving the rules violations
        """
        self.gc_log = gc_log
        self.violations = []
        self.__process_output()


    def __add_violation(self, src, line, rule_id, msg):
        """Add a violation"""

        violation = Violation(src, line, rule_id, msg)
        self.violations.append(violation)

    def __parse_line(self, line):
        """Parse a GnatCheck output line

           Retrieves following informations: source basename, line in
           source, rule identification, violation message removing rubbish
           characters and save those informations as a violation
        """
        #Set maxsplit at 3 because the rule id can contain ':'
        split_1 = line.split(':', 3)
        src = split_1[0]
        src_line = split_1[1]
        # Remove rubbish characters
        split_2 = split_1[-1].split(')', 1)
        rule_id = split_2[0][2:]
        msg = split_2[1].strip()
        self.__add_violation(src, src_line, rule_id, msg)

    def __parse_instance_line(self, line):
        match = re.search('[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:', line)
        msg_split = re.split('[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:', line)

        try:
            start_error = match.group(0).split(':')
            src = start_error[0]
            line = start_error[1]
            ruleid_msg = msg_split[1].split(')', 1)
            rule_id = ruleid_msg[0][2:]
            msg = ruleid_msg[1].strip() + ' (' + msg_split[0] + ' ' + start_error[0] + ')'
            self.__add_violation(src, line, rule_id, msg)

        except IndexError as e:
            print 'Unable to retrieve information from error:\n' + line + 'Skipping this error...'

    def __process_output(self):
        """Process GnatCheck output

          Retrieve information from  gnatcheck.out file
        """
        print 'Processing GNAT Check output...'
        PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:\s[(].*[)]\s.+'
        prog = re.compile(PATTERN)

        with open(self.gc_log, 'r') as gc_output:
            for line in gc_output.readlines():

                if prog.match(line):
                    self.__parse_line(line)

                if re.match('.*instance at.*', line):
                    self.__parse_instance_line(line)


## ReportExporter #################################################################
##

class ReportExporter(object):
    """Export the GnatCheck output as a formatted report"""

    # Class variable
    REPORT_NAME = 'gnatcheck-report'
    EXTENSION = '.xml'

    def __init__(self, report_path):
        """Initializes the class with the full report path containing
           the name"""
        # To be re-arranged
        if not report_path.endswith('/'):
            report_path += '/'
        self.report_path = report_path + self.REPORT_NAME + self.EXTENSION

    def prettify(self, elem):
        """Return a pretty-printed XML string for the Element"""
        rough_string = ElementTree.tostring(elem, 'utf-8')
        reparsed = minidom.parseString(rough_string)
        return reparsed.toprettyxml(indent="  ")

    def export(self, src_map, gc_output):
        """Export GnatCheck output as an xml report

           This method must change for the non prototype version
           to export the report in the decided format
        """
        print 'Creating XML tree...'
        results = Element('results')

        comment = Comment('Gnatcheck rules violation report')
        results.append(comment)
        for v in gc_output.violations:
            error = SubElement(results, 'error',
                               {'file':src_map.get_src_path(v.src),
                                'directory':src_map.get_src_directory(v.src),
                                'project':src_map.get_src_project(v.src),
                                'line':v.line,
                                'id':v.rule_id,
                                'msg':v.msg})
        prettyXML = self.prettify(results)
        with open(self.report_path, 'w+') as gc_report:
            print 'Creating GNAT Check XML report...'
            gc_report.writelines(prettyXML)


## _parse_command_line ###########################################################
##

def _parse_command_line():
    """Parse the command line"""
    parser = argparse.ArgumentParser(description=
                                     'Gnat Check report generator')
    parser.add_argument('--target=', action="store", dest="target",
                        type=str, help="Absolute path target for the generated GnatCheck report",
                        required=True)
    parser.add_argument('--log=', action="store", dest="gc_log",
                        type=str, help="Absolute path to gnatcheck.log file", required=True)
    parser.add_argument('--json-tree=', action='store', dest='json_tree', type=str,
                        help='Absolute path to json file that contains project tree',
                        required=True)
    return parser.parse_args()


## _entry_point ###########################################################
##

def _entry_point():
    """Script entry point"""
    cmd_line = _parse_command_line()
    src_map = SourceMap(cmd_line.json_tree)
    gc_output = GcOutput(cmd_line.gc_log)
    exporter = ReportExporter(cmd_line.target)
    exporter.export(src_map, gc_output)


## Script Entry Point ######################################################
##

if __name__ == '__main__':
    """Calls the entry point and uses its return value as exit code."""
    _entry_point()
