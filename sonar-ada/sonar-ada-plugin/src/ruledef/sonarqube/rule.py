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

"""Mimic SonarQube rule-associated classes"""

import logging
import os
import sys

import lxml.builder
import lxml.etree

from enum import Enum

# All the supported severity values, ordered from INFO to BLOCKER
Severity = Enum('Severity', 'INFO MINOR MAJOR CRITICAL BLOCKER')


class Rule(object):
    """Declare a coding rule

    This object is suitable to be inserted in a :class:`set`.

    :type __key: str
    :type __name: str
    :type tags: list[str] | tuple[str] | str | None
    :type severity: Severity | None
    :type description: str | None
    """

    def __init__(self, key, name, tags=None, severity=None, description=None):
        self.__key = key
        self.__name = name
        self.severity = severity
        self.description = description

        if tags is None:
            self.tags = tuple()
        elif isinstance(tags, basestring):
            self.tags = (tags,)
        else:
            assert isinstance(tags, list) or isinstance(tags, tuple)
            self.tags = tuple(tags)

    def __hash__(self):
        return hash(self.__key)

    def __eq__(self, other):
        return self.__key == other.__key

    def __cmp__(self, other):
        return cmp(self.__key, other.__key)

    def __str__(self):
        return self.__serialize()

    def __repr__(self):
        return self.__serialize(fqn=True)

    def __serialize(self, fqn=False):
        """Retun a string representation of this class

        :param fqn: whether to use the fully qualified name or the simple name
        :type fqn: bool
        :rtype: str
        """
        return '<{} key="{}" name="{}">'.format(
            '{}.{}'.format(self.__class__.__module__, self.__class__.__name__)
            if fqn else self.__class__.__name__, self.__key, self.__name)

    @property
    def key(self):
        """Return the rule key

        :rtype: str
        """
        return self.__key

    @property
    def name(self):
        """Return the rule name

        :rtype: str
        """
        return self.__name


class RulesDefinition(set):
    """Define some coding rules of the same repository

    For example the Java Findbugs plugin provides an implementation of this
    extension point in order to define the rules that it supports.

    :type __repository_key: str
    :type default_severity: Severity | None
    """

    def __init__(self, repository_key, default_severity=None):
        self.__repository_key = repository_key
        self.default_severity = default_severity

    def __str__(self):
        return self.__serialize()

    def __repr__(self):
        return self.__serialize(fqn=True)

    def __serialize(self, fqn=False):
        """Retun a string representation of this class

        :param fqn: whether to use the fully qualified name or the simple name
        :type fqn: bool
        :rtype: str
        """
        output = [
            '{}(repository_key="{}", count="{}", rules=['.format(
                '{}.{}'.format(self.__class__.__module__,
                               self.__class__.__name__)
                if fqn else self.__class__.__name__,
                self.repository_key, len(self)
            )
        ]
        output.append(',{}'.format(os.linesep).join(
            ('  {}'.format(rule) for rule in self)
        ))
        output.append('])')
        return os.linesep.join(output)

    @property
    def repository_key(self):
        """Return the repository key

        :rtype: str
        """
        return self.__repository_key

    def update(self, rules):
        """Update rules in the set

        Return the object to allow methods chaining.

        :param rules: the list of rules to add to the repository
        :type rule: list[Rule]
        :rtype: RulesDefinition
        """
        for rule in rules:
            if rule in self:
                raise KeyError('rule already exists: {}'.format(rule))
            super(RulesDefinition, self).add(rule)
        return self


class RulesDefinitionXmlWriter(object):
    """Generate rules definition XML files from :class:`RulesDefinition`"""

    def write(self, rules, stream=sys.stdout):
        """Write the rules definition to XML into `stream`

        :param rules: the rules definition
        :type rules: RulesDefinition
        :param stream: the output stream
        :type stream: file
        """

        builder = lxml.builder.ElementMaker()
        dom = builder.rules()
        for rule in rules:
            dom.append(self.__create_rule(rule, builder))
        stream.write(lxml.etree.tostring(
            dom, pretty_print=True, xml_declaration=True, encoding='UTF-8'))

    def __create_rule(self, rule, builder):
        """Create a single XML <rule> node

        :param rule: the rule to serialize
        :type rule: Rule
        :param builder: the XML node builder
        :type builder: lxml.builder.ElementMaker
        :rtype: lxml.etree.Element
        """
        node = builder.rule(
            builder.key(rule.key),
            builder.name(rule.name),
            builder.description(rule.description)
        )
        if rule.severity is not None:
            node.append(builder.severity(rule.severity.name))
        for tag in rule.tags:
            node.append(builder.tag(tag))
        return node


class RulesDefinitionXmlReader(object):
    """Load rules definition XML files into :class:`RulesDefinition`

    :type log: logging.Logger
    """

    def __init__(self):
        self.log = logging.getLogger(self.__class__.__name__)

    def load(self, rules, filename):
        """Load definition of rules from an XML file

        :param rules: the rules repository in which to store the loaded rules
        :type rules: RulesDefinition
        :param filename: the XML file containing the rules definition
        :type filename: str
        """
        dom = lxml.etree.parse(filename)
        for node in dom.findall('rule'):
            rules.add([self.__load_rule(node, dom)])

    def __load_rule(self, node, dom):
        """Load a rule from an XML node

        :param node: the XML <rule> node
        :type node: str
        :param dom: the XML dom root
        :type dom: lxml.etree.ElementTree
        :rtype: Node
        """
        assert node.tag == 'rule', 'expected a <rule> node'

        payload = {
            'key': None,
            'name': None,
            'description': None,
            'tags': [],
            'severity': None
        }

        for child in node:
            if child.tag == 'tag':
                payload['tags'].append(child.text)
            elif child.tag == 'severity':
                payload['severity'] = Severity[child.text]
            elif child.tag in payload:
                payload[child.tag] = child.text
            else:
                self.log.warn(
                    'discarding unknown tag <{}> ({}:{})'.format(
                        child.tag, dom.docinfo.URL, child.sourceline))

        return Rule(**payload)


class RulesProfileXmlWriter(object):
    """Generate rules profile XML files"""

    def write(self, language, name, rules_definitions, stream=sys.stdout):
        """Write the rules profile to XML into `stream`

        :param language: the language targeted by the rules profile
        :type language: str
        :param name: the name of the rules profile
        :type name: str
        :param rules_definitions: the collection of :class:`RulesDefinition` to
            activate in the profile
        :type rules_definitions: list[RulesDefinition] | tuple[RulesDefinition]
        :param stream: the output stream
        :type stream: file
        """

        builder = lxml.builder.ElementMaker()
        dom = builder.profile(
            builder.name(name),
            builder.language(language.lower())
        )
        rules = builder.rules()
        for definition in rules_definitions:
            for rule in definition:
                rules.append(builder.rule(
                    builder.key(rule.key),
                    builder.repositoryKey(definition.repository_key)
                ))
        dom.append(rules)
        stream.write(lxml.etree.tostring(
            dom, pretty_print=True, xml_declaration=True, encoding='UTF-8'))
