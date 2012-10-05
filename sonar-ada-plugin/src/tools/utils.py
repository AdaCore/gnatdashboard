#! /usr/bin/env python

from xml.etree import ElementTree
from subprocess import Popen, PIPE, STDOUT
from xml.etree.ElementTree import (Element,
                                   SubElement,
                                   Comment)
from xml.dom import minidom
import os

#############
# CONSTANTS #
#############

PRIORITIES = {'default' : 'MAJOR'}
RESOURCES_PATH='../main/resources'


## Rule #######################################################################
##
class Rule(object):

    def __init__(self, key=None, name=None, description=None):
        self.key = key
        self.configkey = key
        self.name = name
        self.description = description
        self.priority = None


## Rule Repository#############################################################
##
class RuleRepository(object):
    def __init__(self, repository_key, comment):
        self.repository_key = repository_key
        self.comment = comment
        self.rules = []


## __prettify ##################################################################
##
# To be changed: according to snoar xml format style.
def prettify(xml_element):
    """Return a pretty-printed XML string for the Element"""
    rough_string = ElementTree.tostring(xml_element, 'utf-8')
    reparsed = minidom.parseString(rough_string)
    return reparsed.toprettyxml(indent="  ")


## Rule Repository Exporter ###################################################
##
class RuleRepositoryExporter(object):

    def export_rule(self, rule_repository):
        if isinstance(rule_repository, RuleRepository):

            # Create the XML tree
            rules_xml = Element('rules')
            rules_xml.append(Comment(rule_repository.comment))

            for rule in rule_repository.rules:

                rule_xml = SubElement(rules_xml, 'rule')

                key_xml = SubElement(rule_xml, 'key')
                key_xml.text = rule.key

                configkey_xml = SubElement(rule_xml, 'configkey')
                configkey_xml.text = rule.configkey

                name_xml = SubElement(rule_xml, 'name')
                name_xml.text = rule.name

                description_xml = SubElement(rule_xml, 'description')
                description_xml.text = rule.description

                if rule.priority is not None:
                    priority_xml = SubElement(rule_xml, 'priority')
                    priority_xml.text = rule.priority

            prettyrules = prettify(rules_xml)
            with open(os.path.join(os.getcwd(), RESOURCES_PATH, rule_repository.repository_key)  + '.xml', 'w+') as repo_file:
                repo_file.writelines(prettyrules)

        else:
            print 'Could not export rule repository: argument passed to export function is not an instance of RuleRepository object'


class ProfileExporter(object):

    def export_profile(self, rule_repositories):
        pro_file = os.path.join(os.getcwd(), RESOURCES_PATH, 'default-profile.xml')

        profile = Element('profile')
        profile.append(Comment('Dafault Ada Profile '))

        name = SubElement(profile, 'name')
        name.text = 'Default Ada Profile'

        language = SubElement(profile, 'language')
        language.text = 'ada'

        rules = SubElement(profile, 'rules')

        for repository in rule_repositories:
            for rule in repository.rules:
                rule_xml = SubElement(rules, 'rule')

                repositorykey = SubElement(rule_xml, 'repositoryKey')
                repositorykey.text = repository.repository_key

                key = SubElement(rule_xml, 'key')
                key.text = rule.key

                priority = SubElement(rule_xml, 'priority')
                if rule.priority is not None:
                    priority.text = rule.priority
                else:
                    priority.text = PRIORITIES['default']

        pretty_profile = prettify(profile)
        with open (pro_file, 'w+') as profile_xml:
            profile_xml.writelines(pretty_profile)


