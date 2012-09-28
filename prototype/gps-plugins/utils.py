#! /usr/bin/env python

import json
## Violation #################################################################
##
class Violation(object):
    """Represent a rule violation in GnatCheck analyse"""
    def __init__(self, prj, directory, src, line, rule_key, msg):
        self.prj = prj
        self.directory = directory
        self.src = src
        self.line = line
        self.rule_key = rule_key
        self.msg = msg

    def reprJSON(self):
        return dict(prj=self.prj, directory=self.directory, src=self.src,
                    line=self.line, rule_key=self.rule_key, msg=self.msg)


## Project #################################################################
##
class Project(object):
    """Represent a project"""
    def __init__(self, name):
        self.name = name


## Directory #################################################################
##
class Directory(object):
    """Represent a directory"""
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
    """
    def __init__(self, base_name, directory, project):
        self.base_name = base_name
        self.full_name = directory + '/' + base_name
        self.parent_dir = Directory(directory, project)

    def get_project(self):
        return self.parent_dir.get_parent_name()

    def get_directory(self):
        return self.parent_dir.dir_name

    def get_full_name(self):
        """"""
        return self.full_name


## SourceMap #################################################################
##
class SourceMap(object):
    """Map a source file with its project and directory
    """

    def __init__(self, project_tree_path):
        """Initializes the class
        """
        self.src_map = self.__parse_project_tree(project_tree_path)

    def __parse_project_tree(self, project_tree_path):
        """Parse the json file that contains project tree

           Save the informations in a dictionnary where the key is the
           source basename and the value is the corresponding Source object.
        """
        src_map = dict()
        with open(project_tree_path, 'r') as json_tree:
            output = json_tree.read()
            source_tree = json.loads(output)
            for prj in source_tree:
                for src_dir in source_tree[prj]:
                    for src in source_tree[prj][src_dir]:
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

    def get_source(self, src_name):
        return self.src_map[src_name]


## Report #########################################################################
##
class Report(object):

    def __init__(self):
        self.violations = []

    def add_violation(self, violation):
        self.violations.append(violation)

    def reprJSON(self):
        return dict(error=self.violations)


## ComplexEncoder #################################################################
##
class ComplexEncoder(json.JSONEncoder):
    def default(self, obj):
        return obj.reprJSON()


## ReportExporter #################################################################
##

class ReportExporter(object):

    def export_report(self, report_path, report):
        with open(report_path, 'w+') as json_report:
            json_report.write(json.dumps(report.reprJSON(), cls=ComplexEncoder))

