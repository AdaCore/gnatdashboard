#! /usr/bin/env python

from xml.etree import ElementTree
from utils import (RuleRepositoryExporter,
                   ProfileExporter,
                   RuleRepository,
                   Rule)
import argparse
import re
import os

#############
# CONSTANTS #
#############

REPOSITORY_KEY = 'codepeer'
DOC_FILENAME='messages_and_annotations.txt'
COMMENT = '#### Codepeer rule repository generated from html doc page: messages_and_annotations ####'


## CPDocParser #######################################################################
##
class CPDocParser(object):

    def __init__(self, doc_path):
        self.doc_path = doc_path

    def parse_doc(self, rule_repo):
        with open (os.path.join(self.doc_path,DOC_FILENAME), 'rt') as doc:
            recording = False
            started = False
            rule = Rule()
            beginning = re.compile('\+=*\+=*=\+')
            end_rule = re.compile('\+-*\+-*-\+')

            for line in doc.readlines():
                # End of the table
                if line == '\n':
                    recording = False

                # Beggining of the table
                if beginning.match(line):
                    recording = True

                # When there is a multiple line for a table
                if recording and started and line.startswith('|'):
                    infos = line.split('|')
                    rule.key += ' ' + infos[1].strip()
                    rule.description += ' ' + infos[2].strip()

                # Beginning of a table line
                if recording and not started and line.startswith('|'):
                    infos = line.split('|')
                    rule.key = infos[1].strip()
                    rule.description = infos[2].strip()
                    started = True

                # End of a table line
                if recording and end_rule.match(line):
                    started = False
                    rule.configkey = rule.key
                    rule.name = rule.key
                    rule_repo.rules.append(rule)
                    rule = Rule()


## _parse_command_line #######################################################################
##
def _parse_command_line():
    """Parse the command line"""
    parser = argparse.ArgumentParser(description=
                                       'Codepeer rule repository generator')
    parser.add_argument('--doc=', action='store', dest='doc_path', type=str,
                         help='Absolute path to html codepeer documentation, usally: $GNAT_HOME/share/doc/codepeer/user_guide/html/',
                         required=True)
    return parser.parse_args()


## Script entry point #######################################################################
##
def __main__():
    """Script entry point"""
    rule_exporter = RuleRepositoryExporter()
    profile_exporter = ProfileExporter()
    rule_repo = RuleRepository(REPOSITORY_KEY, COMMENT)
    cmd_line = _parse_command_line()
    doc_parser = CPDocParser(cmd_line.doc_path)

    doc_parser.parse_doc(rule_repo)
    rule_exporter.export_rule(rule_repo)
    profile_exporter.export_profile(rule_repo)

if __name__ == '__main__':
    __main__()
