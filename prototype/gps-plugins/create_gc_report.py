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


## Project #################################################################
##
class Project(object):
    """"""
    def __init__(self, name):
        self.name = name


## Directory #################################################################
##
class Directory(object):
    """"""
    def __init__(self, dir_name, project):
        #Retrieve the name of the directory without the full path
        self.dir_name = dir_name.split('/')[-1]
        self.parent_project = Project(project)

    def get_parent_name(self):
        return self.parent_project.name


## Source #################################################################
##
class Source(object):
    """Represent a source file

      For now a source is represent by its base name, full name and by
      the project it belong. In the future, the project must be externalized
      as a class.
    """
    def __init__(self, base_name, directory, project):
        self.base_name = base_name
        self.full_name = directory + '/' + base_name
        self.parent_dir = Directory(directory,project)

    def get_base_name(self):
        """"""

    def get_project(self):
        return self.parent_dir.get_parent_name()

    def get_directory(self):
        return self.parent_dir.dir_name

    def get_full_name(self):
        """"""
        return self.full_name


## GcOutput #################################################################
##
class GcOutput(object):
    """Represent the ouput of GnatCheck

       Is initialized with the path to the GnatCheck log file wich
       contains the output of GnatCheck, parse it and save all the
       rules violations.
    """

    def __init__(self, gc_log):
        """Initializes the class by processing the GnatCheck output
           and saving the rules violations"""
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
           charachters and save those informations as a violation
        """
        split_1 = line.split(':')
        src = split_1[0]
        src_line = split_1[1]
        # Remove rubbish characters
        split_2 = split_1[-1].split(')')
        rule_id = split_2[0][2:]
        msg = split_2[1][1:-1]
        self.__add_violation(src, src_line, rule_id, msg)

    def __process_output(self):
        """Process GnatCheck output

           Process the first line separatley because it is needed
           to remove the progressing informations in the gnatcheck output
        """
        with open(self.gc_log, 'r') as gc_output:
            first = gc_output.readline().split('\r')
            self.__parse_line(first[-1])
            for line in gc_output.readlines():
                self.__parse_line(line)


## SourceMap #################################################################
##
class SourceMap(object):
    """Map a source file with its project and directory

       Execute qmt.py script to retrieve full source path and the project
       it belongs.
    """

    def __init__(self, gpr_path, script, interpret):
        """Initializes the class

           The required arguments may change for the non prototype version
           since for now the qmt.py is launched by a shell script. So if
           the python script will be directly launched without the shell
           script. It will not be needed to change this class.
        """
        self.gpr_path = gpr_path
        self.interpret = interpret
        self.script = script
        self.src_map = self.__parse_output()

    def __load_script_output(self):
        """Execute the qmt.py script and retrive the output"""
        cmd = self.interpret + ' ' + self.script + ' -d ' + self.gpr_path
        try:
            p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE,
                      stderr=STDOUT, close_fds=True)
        except OSError as os:
             print 'Connot find the script for the given path: ' + gpr_path

        return p.stdout.read()


    def __parse_output(self):
        """Parse the qmt.py script output

           Save the informations in a dictionnary where the key is the
           source basename and the value is a tuple of the project and
           the full path
        """
        src_map = dict()
        output = self.__load_script_output()
        try:
            tree_source = json.loads(output)
        except ValueError:
            print 'Output of script ' + self.script + ' is not a JSon format.'
            print output
            exit(1)
        for prj in tree_source:
            for src_dir in tree_source[prj]:
                for src in tree_source[prj][src_dir]:
                    source = Source(src, src_dir, prj)
                    src_map[src] = source
        return src_map

    def get_src_path(self, src):
        """Return the source full path"""
        return self.src_map[src].get_full_name()

    def get_src_directory(self, src):
        """"""
        return self.src_map[src].get_directory()

    def get_src_project(self, src):
        """Return the project the source blongs"""
        return self.src_map[src].get_project()


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
            gc_report.writelines(prettyXML)


## _parse_command_line ###########################################################
##

def _parse_command_line():
    """Parse the command line"""
    parser = argparse.ArgumentParser(description=
                                     'Gnat Check report generator')
    parser.add_argument('-P', action="store", dest="gpr_path", type=str,
                        help="Root project file, absolute path, to analyse with GnatCheck",
                        required=True)
    parser.add_argument('-S', action="store", dest="script",
                        type=str, help="Path to the script that execute the qmt.py script, e.g: run.sh",
                        required=True)
    parser.add_argument('-I', action="store", dest="interpreter",
                        type=str, help="Interpreter to use for the script, e.g: sh, python",
                        required=True)
    parser.add_argument('--target=', action="store", dest="target",
                        type=str, help="Absolute path target for the generated GnatCheck report",
                        required=True)
    parser.add_argument('--log=', action="store", dest="gc_log",
                        type=str, help="Absolute path to gnatcheck.log file", required=True)
    return parser.parse_args()


## _entry_point ###########################################################
##

def _entry_point():
    """Script entry point"""
    cmd_line = _parse_command_line()
    src_map = SourceMap(cmd_line.gpr_path, cmd_line.script,
                        cmd_line.interpreter)
    gc_output = GcOutput(cmd_line.gc_log)
    exporter = ReportExporter(cmd_line.target)
    exporter.export(src_map, gc_output)


## Script Entry Point ######################################################
##

if __name__ == '__main__':
    """Calls the entry point and uses its return value as exit code."""
    _entry_point()
