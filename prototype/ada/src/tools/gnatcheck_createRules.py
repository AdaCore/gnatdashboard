#! /usr/bin/env python

"""Generate XML gnat check rule repository for Ada sonar plugin

Retreive rule informations from gnatcheck -hx to generate the
rule repository and the default profile for the Sonar Ada plugin.
Details in functions\'s documentation.
"""
from xml.etree import ElementTree
from subprocess import Popen, PIPE, STDOUT
from xml.etree.ElementTree import (Element,
                                   SubElement,
                                   Comment)
from xml.dom import minidom
import argparse
import os.path
import os


labels = dict()
PRIORITIES = {'default' : 'MAJOR'}
REPOSITORY_KEY = 'gnatcheck'
TMP_FILE = 'allRules.xml'


def removeTmpFile():
    if os.path.exists(TMP_FILE): os.remove(TMP_FILE)


def importRulesInfos():
    """Retreive rule informations from gnatcheck -hx

    Attribute "switch" correspond to the rule key
    Attribute "label" correspond to the rule name
    Skipping "Category" and parameters information.
    """
    # Write rules in a temporary file with gnat check rules
    cmd = 'gnatcheck -hx'
    p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
    output = p.stdout.read()
    with open(TMP_FILE, 'w+') as allrules:
        allrules.writelines(output)
    # Parse the temporary file to retreive rules's informations
    with open(TMP_FILE, 'rt') as allrules:
        try:
            tree = ElementTree.parse(allrules)
            for path in ['.//check', './/spin', './/field']:
                for node in tree.findall(path):
                    ruleid = node.attrib.get('switch')
                    label = node.attrib.get('label')
                    if ruleid and label:
                        labels[ruleid[2:]] = label
        except Exception as e:
            print "Unable to parse gnatcheck rules:"
            print e.message
            allrules.seek(0)
            # Display the unparsable line
            print allrules.readlines()[e.position[0] - 1]
            removeTmpFile()
    # Remove the temporary file
    removeTmpFile()

# To be changed: according to snoar xml format style.
def prettify(elem):
    """Return a pretty-printed XML string for the Element"""
    rough_string = ElementTree.tostring(elem, 'utf-8')
    reparsed = minidom.parseString(rough_string)
    return reparsed.toprettyxml(indent="  ")


def exportRules():
    """Create an XML rule repository for sonar"""
    # Create the XML tree
    rules = Element('rules')
    comment = Comment('Genrated from gnatcheck -hx')
    rules.append(comment)
    for ruleid in labels:
        rule = SubElement(rules, 'rule')
        key = SubElement(rule, 'key')
        key.text = ruleid
        confkey = SubElement(rule, 'configkey')
        confkey.text = ruleid
        name = SubElement(rule, 'name')
        name.text = labels[ruleid]
    # Write in the file
    prettyrules = prettify(rules)
    with open('../main/resources/' + REPOSITORY_KEY  + '.xml', 'w+') as rulerepo:
        #rulerepo.write(XML_HEADER)
        rulerepo.writelines(prettyrules)


def exportProfile():
    """Create a default Ada Profile for sonar plugin

    A profile is a set of rules where each rule is associated to a priority.
    Here the default priority is associated to all the rules, which is MAJOR.
    """
    # Create the XML tree
    profile = Element('profile')
    comment = Comment('Dafault Ada Profile generate from gnatcheck -hx')
    profile.append(comment)
    name = SubElement(profile, 'name')
    name.text = 'Default Ada Profile'
    language = SubElement(profile, 'language')
    language.text = 'ada'
    rules = SubElement(profile, 'rules')
    for ruleid in labels:
        rule = SubElement(rules, 'rule')
        repositorykey = SubElement(rules, 'repositoryKey')
        repositorykey.text = REPOSITORY_KEY
        key = SubElement(rules, 'key')
        key.text = ruleid
        priority = SubElement(rules, 'priority')
        priority.text = PRIORITIES['default']
    # Write in the file
    prettyprofile = prettify(profile)
    with open ('../main/resources/default-profile.xml', 'w+') as defprofile:
        #defprofile.write(XML_HEADER)
        defprofile.writelines(prettyprofile)


def __main__():
    importRulesInfos()
    exportRules()
    exportProfile()

if __name__ == '__main__':
    __main__()
