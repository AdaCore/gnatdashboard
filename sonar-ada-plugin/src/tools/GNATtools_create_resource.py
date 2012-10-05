#! /usr/bin/env python

from utils import (RuleRepositoryExporter,
                   ProfileExporter,
                   RuleRepository)
from cp_parser import CPParserExecutor
from gc_parser import GCParserExecutor
import argparse

#############
# CONSTANTS #
#############

GNAT_TOOLS = {'gnatcheck' : GCParserExecutor(),
              'codepeer' : CPParserExecutor()}


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
    cmd_line = _parse_command_line()
    rule_repositories = []
    profile_exporter = ProfileExporter()
    for tool in GNAT_TOOLS:
        print 'Executing ' + tool + ' parser...'
        rule_exporter = RuleRepositoryExporter()

        rule_repository = GNAT_TOOLS[tool].execute_parser(cmd_line)
        rule_exporter.export_rule(rule_repository)
        rule_repositories.append(rule_repository)

    profile_exporter.export_profile(rule_repositories)
    print '-- DONE --'

if __name__ == '__main__':
    __main__()

