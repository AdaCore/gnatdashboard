#! /usr/bin/env python

"""Generate XML gnat check rule repository for Ada sonar plugin

Retreive rule informations from gnatcheck -hx to generate the
rule repository. Details in functions\'s documentation.
"""
from xml.etree import ElementTree
from subprocess import Popen, PIPE, STDOUT
from xml.etree.ElementTree import (Element,
                                   SubElement,
                                   Comment)
from xml.dom import minidom
import os

labels = dict()

def importRulesInfos():
    """Retreive rule informations from gnatcheck -hx

    Attribute "switch" correspond to the rule key
    Attribute "label" correspond to the rule name
    Skipping "Category" and parameters information.
    """
    with open('allRules.xml', 'rt') as allrules:
        try:
            tree = ElementTree.parse(allrules)
            for path in ['.//check', './/spin', './/field']:
                for node in tree.findall(path):
                    ruleid = node.attrib.get('switch')
                    label = node.attrib.get('label')
                    if ruleid and label:
                        labels[ruleid[2:]] = label
        except Exception as e:
            print "Unable to parse gnatcheck rules."
            print "Execution of gnatcheck -hx returns with the following error : "
            allrules.seek(0)
            print allrules.readlines()[e.position[0] - 1]
            os.remove("allRules.xml")


def prettify(elem):
    """Return a pretty-printed XML string for the Element"""
    rough_string = ElementTree.tostring(elem, 'utf-8')
    reparsed = minidom.parseString(rough_string)
    return reparsed.toprettyxml(indent="  ")


def exportRules():
    """Create an XML rule repository for sonar"""
    #Create the XML tree
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
    #Write in the file
    prettyrules = prettify(rules)
    with open('../main/resources/gnatcheck.xml', 'w+') as rulerepo:
        rulerepo.writelines(prettyrules)

def __main__():
    #Create temporary file with gnatcheck rules
    cmd = 'gnatcheck -hx'
    p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
    output = p.stdout.read()
    with open('allRules.xml', 'w+') as allrules:
        allrules.writelines(output)
    importRulesInfos()
    exportRules()
    os.remove("allRules.xml")

if __name__ == '__main__':
    __main__()
