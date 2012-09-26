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
DOC_FILENAME='messages_and_annotations.rst'
COMMENT = '#### Codepeer rule repository generated from html doc page: messages_and_annotations ####'


## CPDocParser #######################################################################
##
class CPDocParser(object):
    """Represent parser for codepeer documentation

    Constructor takes the path to the documention
    as parameter.
    """
    def __init__(self, doc_path):
        self.doc_path = doc_path

    def parse_doc(self, rule_repo):
        """Parse codepeer documentation

        Parse documentation in the given location. Some details about the algo:
        recording: assigned to True when the line match the beggining of the
                   rule table.
                   assigned to False when line is \n,identified the end of table
        started: assigned to True when starts saving info of the first line of a
                 rules table line.
        """
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

                # Beginning of a table line
                if recording and not started and line.startswith('|'):
                    infos = line.split('|')
                    rule.key = infos[1].strip()
                    rule.description = infos[2].strip()
                    started = True
                    continue

                # When there is a multiple line for a table
                if recording and started and line.startswith('|'):
                    print line
                    infos = line.split('|')
                    rule.key += ' ' + infos[1].strip()
                    rule.description += ' ' + infos[2].strip()

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
    """Parse the command line

    Retrieve path to rst codepeer user guide documentation
    """
    parser = argparse.ArgumentParser(description=
                                       'Codepeer rule repository generator')
    parser.add_argument('--doc=', action='store', dest='doc_path', type=str,
                         help='Absolute path to rst codepeer user guide documentation',
                         required=True)
    return parser.parse_args()


## Script entry point #######################################################################
##
def __main__():
    """Script entry point

    Creates rule repository and append the defalut profile with the rules.
    This entry point must be outsourced to drive the generation of all rule
    repository (for every integrated tool).
    """
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
