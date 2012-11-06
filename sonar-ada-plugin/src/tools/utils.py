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
    """Represents a Sonar rule."""

    def __init__(self, key=None, name=None, description=None, priority=None):
        """Initializes a rule.

        Keyword arguments:
        key -- rule identifier
        configey -- equals to the key (this member needs investigation)
        name -- name of the rule, displayed in Sonar GUI.
        description -- description of the rule, displayed in the Sonar GUI
        priority -- default priority of the rule
        """
        self.key = key
        self.configkey = key
        self.name = name
        self.description = description
        self.priority = priority


## Rule Repository#############################################################
##
class RuleRepository(object):
    """Represents a Sonar rule repository"""
    def __init__(self, repository_key, comment):
        """Initializes a rule repository

        Keyword arguments:
        repository_key -- indentifier of a the rule repository
        comment -- top comment on the rule repository xml file
        """
        self.repository_key = repository_key
        self.comment = comment
        self.rules = []


## prettify ###################################################################
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
    """Exporter for Sonar rule repository"""

    def export_rule(self, rule_repository):
        """Exports object representation of Sonar rule repository to xml file

        Keyword arguments:
        rule_repository -- Object representation of Sonar rule repository.
        """
        # Check type of the argument
        if isinstance(rule_repository, RuleRepository):

            # Creates the XML tree
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

                # Add priority field if exists
                if rule.priority is not None:
                    priority_xml = SubElement(rule_xml, 'priority')
                    priority_xml.text = rule.priority

            #Dumps the xml file
            prettyrules = prettify(rules_xml)
            with open(os.path.join(os.getcwd(), RESOURCES_PATH, rule_repository.repository_key)  + '.xml', 'w+') as repo_file:
                repo_file.writelines(prettyrules)

        else:
            print 'Could not export rule repository: argument passed to export function is not an instance of RuleRepository object'


## Profile Exporter ###########################################################
##
class ProfileExporter(object):
    """Exporter for default Sonar quality profile """

    def export_profile(self, rule_repositories):
        """Exports Sonar profile to an xml file

        Keyword arguments:
        rule_repositories -- all the rule repositories that the profile
                             must contains.
        """
        pro_file = os.path.join(os.getcwd(), RESOURCES_PATH, 'default-profile.xml')

        # Creates the XML tree
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


