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
COMMENT = '#### Codepeer rule repository generated from html doc page: messages_and_annotations ####'
PRIORTIES_LIST = {'CRITICAL', 'MAJOR', 'MINOR', 'INFO'}
CATEGORIES_LIST = {'CHECK', 'WARNING', 'RACE CONDITION', 'INFORMATIONAL'}

## CPDocParser #################################################################
##
class CPDocParser(object):
    """Represent parser for Codepeer documentation.
    """
    def __init__(self, doc_path):
        """Initializes CPDocParser

        Keyword arguments:
        doc_path -- path to the Codepeer documention directory.
                    Doc is parsed to retrieve all Codepeer messages (rules)
                    with its description.
        """
        self.doc_path = doc_path

    def parse_doc(self, rule_repo):
        """Parse codepeer documentation.

        Keyword arguments:
        rule_repo -- instance of utils.RuleRepository object.
                   The method will fill RuleRepository#rules by creating Sonar
                   rules from retrieved Codepeer messages.
                   For each messages 4*4 Sonar rules are created, corresponding
                   to each association of: categories and severities.

        Some details about the algo:
        recording: assigned to True when the line match the beggining of a rule
                   table. Assigned to False when line=\n: identified the end of
                   table.
        started: assigned to True when starts saving info of the first line of
                 a rules table line.
        """
        with open (self.doc_path, 'rt') as doc:
            # Initialisation
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
                    #Because it is not possible to set the severity of a
                    #violation, it is needed to create a rule for each
                    #severity. See TN [L919-022]
                    for category in CATEGORIES_LIST:
                        for priority in PRIORTIES_LIST:
                            new_rule = Rule(rule.key, rule.name, rule.description, priority)
                            new_rule.key = priority + ':' + category + ':' + new_rule.key.strip()
                            new_rule.configkey = new_rule.key
                            rule_repo.rules.append(new_rule)

                    rule = Rule()

## CPParserExecutor ############################################################
##
class CPParserExecutor(object):
    """Represents executor for Codepeer Parser"""

    def execute_parser(self, cmd_line):
        """Executes Codepeer Parser

        Keyword arguments:
        cmd_line -- allows to retieve Codepeer doc directory path, passed
                    in the command line.

        Initializes a RuleRepository and a CPDocParser, then executes the
        Codepeer parser.
        Returns the RuleRepository instance.
        """
        rule_repo = RuleRepository(REPOSITORY_KEY, COMMENT)
        doc_parser = CPDocParser(cmd_line.cp_doc_path)

        doc_parser.parse_doc(rule_repo)

        return rule_repo

