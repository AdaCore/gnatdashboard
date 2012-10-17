#! /usr/bin/env python

from xml.etree import ElementTree
from utils import (RuleRepositoryExporter,
                   ProfileExporter,
                   RuleRepository,
                   Rule)
import re
import os

#############
# CONSTANTS #
#############

REPOSITORY_KEY = 'codepeer'
DOC_FILENAME='messages_and_annotations.rst'
COMMENT = '#### Codepeer rule repository generated from html doc page: messages_and_annotations ####'
PRIORTIES_LIST = {'CRITICAL', 'MAJOR', 'MINOR', 'INFO'}


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
                    infos = line.split('|')
                    rule.key += ' ' + infos[1].strip()
                    rule.description += ' ' + infos[2].strip()

                # End of a table line
                if recording and end_rule.match(line):
                    started = False
                    rule.name = rule.key.strip()
                    #Because it is not possible to set the priorit of a
                    #violation, it is needed to create a rule for each
                    #priority. See TN [L919-022]
                    for priority in PRIORTIES_LIST:
                        new_rule = Rule(rule.key, rule.name, rule.description)
                        new_rule.key = new_rule.key.strip() + ' - ' + priority
                        new_rule.configkey = new_rule.key
                        new_rule.priority = priority
                        rule_repo.rules.append(new_rule)

                    rule = Rule()

## CPParserExecutor #######################################################################
##
class CPParserExecutor(object):
    def execute_parser(self, cmd_line):
        rule_repo = RuleRepository(REPOSITORY_KEY, COMMENT)
        doc_parser = CPDocParser(cmd_line.cp_doc_path)

        doc_parser.parse_doc(rule_repo)

        return rule_repo

