#! /usr/bin/env python

from xml.etree import ElementTree
from subprocess import Popen, PIPE, STDOUT
from xml.etree import ElementTree
from xml.dom import minidom
from utils import (RuleRepositoryExporter,
                   ProfileExporter,
                   RuleRepository,
                   Rule)
import tempfile
import argparse
import os.path
import os
import re

#############
# CONSTANTS #
#############

REPOSITORY_KEY = 'gnatcheck'
COMMENT = '### GNATCheck rule repository generated from gnatcheck switch: gnatcheck -hx ###'

## GCParser #################################################################
##
class GCParser(object):
    """Retrieve rule informations from gnatcheck -hx

    Attribute "switch" correspond to the rule key
    Attribute "label" correspond to the rule name
    Skipping "Category" and parameters information.
    """

    def __init__(self, doc_path):
        """Initializes GCParser

        Keyword arguments:
        doc_path -- path to the GNATcheck documentation.
                    Doc is parsed to retrieve GNATcheck rule's
                    description.

        Initializes doc_by_rule dictionnary by parsing GNATcheck
        documentation.
        doc_by_rule:
          - key = rule key
          - value = rule description.
        """
        self.doc_path = doc_path
        self.doc_by_rule = self.parse_doc()

    def parse_output(self, rule_repo):
        """Parse gnatcheck -hx output.

        Keyword arguments:
        rule_repo -- instance of utils.RuleRepository object.
                     This method will fill RuleRepository#rules by creation
                     Sonar rules from retrieved GNATcheck output and
                     documentation.
        """
        # Executes gnatcheck -hx and puts the result in a temporary file.
        cmd = 'gnatcheck -hx'
        p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
        output = p.stdout.read()
        (fd, file_name) = tempfile.mkstemp(suffix='.xml', prefix='gc_rules')

        with open(file_name, 'w+') as gc_rules:
            gc_rules.writelines(output)

        # Parses the temporary xml file
        with open(file_name, 'rt') as rules:
            try:
                tree = ElementTree.parse(rules)

                for path in ['.//check', './/spin', './/field']:
                    for node in tree.findall(path):
                        rule_key = node.attrib.get('switch')
                        label = node.attrib.get('label')
                        if rule_key and label:
                            key = rule_key[2:]
                            #key.split(':') concerns rule with specific category.
                            rule = Rule(key, label,
                                      self.doc_by_rule[key.split(':')[0]])
                            rule_repo.rules.append(rule)

            except Exception as e:
                print e.message

            finally:
                if os.path.exists(file_name): os.remove(file_name)

    def parse_doc(self):
        """Parse GNATcheck documentation

        Returns a dictionnary to initializes doc_by_rule class member.
        """
        DOC_TITLE = '[89](\.?[0-9]{0,2}){0,2} [a-zA-Z-`\' 0-9]'
        DOC_RULE_TITLE = '8\.[1-9](\.[0-9]{0,2})+ `[a-zA-Z_]+\''
        prog_title = re.compile(DOC_TITLE)
        prog_rule = re.compile(DOC_RULE_TITLE)
        doc_by_rule = dict()

        with open (self.doc_path, 'r') as doc:

            description = ''
            rule_key = ''
            recording = 0

            for line in doc.readlines():

              # if line matches a rule title,
              #  if the line followed a rule paragrpah: saves previous desc
              # In any case retrieve the rule key and affect 1 to recording
                if prog_rule.match(line):
                    if description != '':
                        doc_by_rule[rule_key] = description.strip()
                        description = ''
                    rule_key = line.strip().split(' ')[1][1:-1].strip()
                    recording = 1

                # Skip the decoration line under the rule title
                elif recording == 1:
                    recording += 1

                # If the line matches a general title: end of desc paragraph,
                #   - saves desc and rule key in dictionnary
                #   - re-initializes all variables
                elif prog_title.match(line):
                    if rule_key != '' and description != '':
                        doc_by_rule[rule_key] = description.strip()
                    recording = 0
                    description = ''
                    rule_key = ''

                # Line is in a rule description paragraph: saves it
                elif recording == 2:
                    description += line

        return doc_by_rule


## GCParserExecutor #######################################################################
##
class GCParserExecutor(object):
    """Represents executor for GNATcheck Pasrser"""

    def execute_parser(self, cmd_line):
        """Executes GNATcheck Parser

        Keyword arguments:
        cmd_line -- allows to retrieve GNATcheck path documentation,
                    passed in the command line.

        Initializes a RuleRepository and a GCParser, then executes the
        GNATcheck parser.
        Returns the RuleRepository instance.
        """
        rule_repo = RuleRepository(REPOSITORY_KEY, COMMENT)
        gc_parser = GCParser(cmd_line.gc_doc_path)

        gc_parser.parse_output(rule_repo)

        return rule_repo

