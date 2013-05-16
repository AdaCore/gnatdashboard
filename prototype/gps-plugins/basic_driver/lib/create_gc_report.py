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
import os
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
        """Initializes the object by processing the GnatCheck output

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
        # Example with line : "input.adb:3:19: use clause for package
        # [USE_PACKAGE_Clauses]"

        # Set maxsplit at 3 because the rule id can contain ':'
        # Result : ['input.adb', '3', '19', ' use clause for package
        # [USE_PACKAGE_Clauses]']
        split_1 = line.split(':', 3)
        src = split_1[0]
        src_line = split_1[1]

        # Remove rubbish characters
        # Result : [' use clause for package ', 'USE_PACKAGE_Clauses]']
        split_2 = split_1[-1].split('[', 1)
        # Remove the closing brace.
        rule_id = split_2[1].strip()[:-1]
        msg = split_2[0].strip()
        self.__add_violation(src, src_line, rule_id, msg)

    def __parse_instance_line(self, line):
        MAIN_SRC_FILE = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:'
        # Look for the source file that instanciates the generic (last un the
        # list of "instance at...")
        match = re.search(MAIN_SRC_FILE, line)
        # Split to have a list with 'commands-generic_asynchronous.ads:57:15
        # instance at...' and 'message + rule id'
        msg_split = re.split(MAIN_SRC_FILE, line)

        try:
            # Retreive info on the main file
            # Result : ['vsearch.adb', '231', '4', '']
            start_error = match.group(0).split(':')
            src = start_error[0].strip()
            line = start_error[1]
            ruleid_msg = msg_split[1].split('[', 1)
            rule_id = ruleid_msg[1].strip()[:-1]
            msg = ruleid_msg[0].strip() + ' (' + msg_split[0].strip() + ' ' + src + ')'
            self.__add_violation(src, line, rule_id, msg)

        except IndexError as e:
            print 'Unable to retrieve information from error: '
            print line + '\nSkipping this error...'

    def __process_output(self):
        """Process GnatCheck output

          Retrieve information from  gnatcheck.out file
        """
        print 'Processing GNAT Check output...'
        ERROR_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+:\s.+[[].*[]]\s*'
        INSTANCE_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+:[0-9]+ instance at [a-zA-Z-_.0-9]+:[0-9]+:[0-9]+.+'
        prog_error = re.compile(ERROR_PATTERN)
        prog_instance = re.compile(INSTANCE_PATTERN)

        with open(self.gc_log, 'r') as gc_output:
            for line in gc_output.readlines():

                if prog_error.match(line):
                    self.__parse_line(line)

                if prog_instance.match(line):
                    self.__parse_instance_line(line)


## ReportExporter #############################################################
##

class ReportExporter(object):
    """Export the GnatCheck output as a formatted report"""

    REPORT_NAME = 'gnatcheck-report.xml'

    def __init__(self, report_path):
        """Initializes the class with the full report path containing
           the name"""
        self.report_path = os.path.join(report_path, self.REPORT_NAME)

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
            try:
                src = src_map.get_path(v.src)
                directory = src_map.get_directory(v.src)
                project = src_map.get_project(v.src)
                error = SubElement(results, 'error',
                                   {'file': src,
                                    'directory': directory,
                                    'project': project,
                                    'line': v.line,
                                    'id': v.rule_id,
                                    'msg': v.msg})
            except KeyError as e:
                print 'Skipping error, unable to find source in project tree: ' + v.src

        pretty_xml = self.prettify(results)
        with open(self.report_path, 'w+') as gc_report:
            print 'Creating GNAT Check XML report...'
            gc_report.writelines(pretty_xml)


## _parse_command_line ########################################################
##

def _parse_command_line():
    """Parse the command line"""
    parser = argparse.ArgumentParser(description=
                                     'Gnat Check report generator')
    parser.add_argument('--report-dir=', action="store", dest="target",
                        type=str, help='Absolute path target for the' +
                        'generated GnatCheck report', required=True)
    parser.add_argument('--output=', action="store", dest="gc_log",
                        type=str, help="Absolute path to gnatcheck.log file",
                        required=True)
    parser.add_argument('--json-tree=', action='store', dest='json_tree',
                        type=str, help='Absolute path to json file that' +
                        'contains project tree', required=True)
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
