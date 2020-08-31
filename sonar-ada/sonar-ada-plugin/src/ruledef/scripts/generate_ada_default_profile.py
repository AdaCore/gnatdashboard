#! /usr/bin/env python

# Sonar Ada Plugin (GNATdashboard)
# Copyright (C) 2015, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

"""Generate :file:`default-profile.xml` containing the rules profile for Ada

SonarQube's plugin `sonar-ada-plugin` loads the Ada rules profile by reading
the XML files in which the rules are listed. This script generates such XML
file from the various input rules definition XML files.
"""

import logging
import sys
from argparse import ArgumentParser, FileType

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError

Tools = ['gnatcheck', 'codepeer', 'gnatcoverage', 'spark2014']


def __print_header(language, name):
    # Print the header
    print("""<?xml version='1.0' encoding='UTF-8'?>

<profile>
   <name>{}</name>
   <language>{}</language>
   <rules>""".format(name, language.lower()))


def __print_footer():
    # Print the footer
    print("""   </rules>
</profile>""")


def __print_rule(def_profile_rule):
    print("""      <rule>
        <key>{}</key>
        <repositoryKey>{}</repositoryKey>
      </rule>""".format(def_profile_rule['key'],
                        def_profile_rule['repositoryKey']))


def __load_rules(rules_definition_files):
    """Parse the input rules definition XML files

    :param rules_definition_files: the files containing rules definitions
    :type rules_definition_files: list[str]
    :rtype: ada_default_rules: list[{str, str}]
    """

    for filename in rules_definition_files:
        try:
            tree = ElementTree.parse(filename).getroot()

            # Fetch all rules and fill default rule information
            for rule in tree.findall('./rule'):
                default_rule = {
                    'key': None,
                    'repositoryKey': None
                }

                for child in rule:
                    if child.tag == 'key':
                        default_rule['key'] = child.text
                    else:
                        if child.tag == 'tag' and child.text in Tools:
                            default_rule['repositoryKey'] = child.text
                __print_rule(default_rule)

        except ParseError:
            logging.INFO('failed to parse XML file %s' % filename)


def generate_ada_rules_profile():
    """Parse the command line and generate the XML file"""

    cmdline = ArgumentParser(description='Ada rules profile generator')
    cmdline.add_argument('-l', '--language', default='Ada')
    cmdline.add_argument('-n', '--name', default='GNATdashboard way')
    cmdline.add_argument(
        'rules', nargs='+', metavar='rules-definition',
        help='one or more rules definition file(s)')
    cmdline.add_argument(
        '-o', '--output', default=sys.stdout, dest='outfile',
        type=FileType('w'), help='Output file')
    params = cmdline.parse_args()

    __print_header(params.language, params.name)
    __load_rules(params.rules)
    __print_footer()
    return 0


if __name__ == '__main__':
    logging.basicConfig(
        level=logging.INFO,
        format='[%(asctime)s] %(levelname)s - %(message)s',
        stream=[logging.StreamHandler()]
    )

    try:
        sys.exit(generate_ada_rules_profile())
    except KeyboardInterrupt:
        sys.exit('Interrupted.')
