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


#############
# CONSTANTS #
#############

REPOSITORY_KEY = 'gnatcheck'
COMMENT = '### GNATCheck rule repository generated from gnatcheck switch: gnatcheck -hx ###'


class GCParser(object):
    """Retrieve rule informations from gnatcheck -hx

    Attribute "switch" correspond to the rule key
    Attribute "label" correspond to the rule name
    Skipping "Category" and parameters information.
    """

    def parse_output(self, rule_repo):
        cmd = 'gnatcheck -hx'
        p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
        output = p.stdout.read()
        (fd, file_name) = tempfile.mkstemp(suffix='.xml', prefix='gc_rules')

        with open(file_name, 'w+') as gc_rules:
            gc_rules.writelines(output)

        with open(file_name, 'rt') as rules:
            try:
                tree = ElementTree.parse(rules)

                for path in ['.//check', './/spin', './/field']:
                    for node in tree.findall(path):
                        rule_key = node.attrib.get('switch')
                        label = node.attrib.get('label')
                        if rule_key and label:
                            rule = Rule(rule_key[2:], label, 'No description')
                            rule_repo.rules.append(rule)

            except Exception as e:
                print "Unable to parse gnatcheck rules:"
                print e.message
                rules.seek(0)
                # Display the unparsable line
                print rules.readlines()[e.position[0] - 1]

            finally:
                if os.path.exists(file_name): os.remove(file_name)


def __main__():
    rule_exporter = RuleRepositoryExporter()
    profile_exporter = ProfileExporter()
    rule_repo = RuleRepository(REPOSITORY_KEY, COMMENT)
    gc_parser = GCParser()

    gc_parser.parse_output(rule_repo)
    rule_exporter.export_rule(rule_repo)
    profile_exporter.export_profile(rule_repo)

if __name__ == '__main__':
    __main__()
