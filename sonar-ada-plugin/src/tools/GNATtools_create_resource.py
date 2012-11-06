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

# GNAT Tools supported by the Sonar Ada plugin
GNAT_TOOLS = {'gnatcheck' : GCParserExecutor(),
              'codepeer' : CPParserExecutor()}


## _parse_command_line ########################################################
##
def _parse_command_line():
    """Parse the command line

    Exepcted arguments:
      - absolute path to rst codepeer user guide documentation directory
      - absolute path to gnatcheck_rm.rst file
    """
    parser = argparse.ArgumentParser(description=
                                       'Codepeer rule repository generator')
    parser.add_argument('--cp_doc=', action='store', dest='cp_doc_path', type=str,
                         help='Absolute path to rst codepeer user guide documentation',
                         required=True)
    parser.add_argument('--gc_doc=', action='store', dest='gc_doc_path', type=str,
                         help='Absolute path to rst gnatcheck_rm documentation',
                         required=True)
    return parser.parse_args()

## Script entry point #########################################################
##
def __main__():
    """ Script entry point

    Uses all *_parser.py script and utils.py script.
    All *_parser.py scripts, correspond to a parser for each supported tools;
    they contain:
      - *Parser object: has a parse method, to parse output, documentation,etc.
      - *ParserExecutor: executes the parser with appropriate arguments.
                         This object has been added in order to allow
                         generic external execution of all GNAT Tool's parser
                         supported (declared in GNAT_TOOLS global var
                         dictionnary).

    This main function:
      - retrieves command line arguments for paths to tool's
        documentation, if necessary.
      - calls "execute_parser" of all defined GNAT Tools ParserExecutor,
      - export rule repository for all defined GNAT Tools
      - export default Sonar quality profile.
    """
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

