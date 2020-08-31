# Sonar Ada Plugin (GNATdashboard)
# Copyright (C) 2015-2017, AdaCore
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

"""Mimic SonarQube rule-associated classes."""

import logging
import os
import sys
from filecmp import cmp

import lxml.builder
import lxml.etree

from enum import Enum

# All the supported severity values, ordered from INFO to BLOCKER
Severity = Enum('Severity', 'INFO MINOR MAJOR CRITICAL BLOCKER')
Type = Enum('Type', 'BUG VULNERABILITY CODE_SMELL')


class Rule(object):
    """Declare a coding rule.

    This object is suitable to be inserted in a :class:`set`.

    :type __key: str
    :type __name: str
    :type tags: list[str] or tuple[str] or str or None
    :type type: Type or None
    :type severity: Severity or None
    :type description: str or None
    """

    def __init__(self, key, name, tags=None, severity=None, type=None,
                 description=None):
        self.__key = key
        self.__name = name
        self.type = type
        self.severity = severity
        self.description = description

        if tags is None:
            self.tags = tuple()
        elif isinstance(tags, str):
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
        """Retun a string representation of this class.

        :param bool fqn: whether to use the fully qualified name or the simple
            name
        :rtype: str
        """
        return '<{} key="{}" name="{}">'.format(
            '{}.{}'.format(self.__class__.__module__, self.__class__.__name__)
            if fqn else self.__class__.__name__, self.__key, self.__name)

    @property
    def key(self):
        """Return the rule key.

        :rtype: str
        """
        return self.__key

    @property
    def name(self):
        """Return the rule name.

        :rtype: str
        """
        return self.__name


class RulesDefinition(set):
    """Define some coding rules of the same repository.

    For example the Java Findbugs plugin provides an implementation of this
    extension point in order to define the rules that it supports.

    :type __repository_key: str
    :type default_severity: Severity or None
    """

    def __init__(self, repository_key, default_severity=None):
        self.__repository_key = repository_key
        self.default_severity = default_severity

    def __str__(self):
        return self.__serialize()

    def __repr__(self):
        return self.__serialize(fqn=True)

    def __serialize(self, fqn=False):
        """Retun a string representation of this class.

        :param bool fqn: whether to use the fully qualified name or the simple
            name
        :rtype: str
        """
        return os.linesep.join([
            '{}(repository_key="{}", count="{}", rules=['.format(
                '{}.{}'.format(
                    self.__class__.__module__, self.__class__.__name__)
                if fqn else self.__class__.__name__,
                self.repository_key, len(self)),
            ',{}'.format(os.linesep).join(
                ('  {}'.format(rule) for rule in self)),
            '])'])

    @property
    def repository_key(self):
        """Return the repository key.

        :rtype: str
        """
        return self.__repository_key

    def add(self, rule):
        """Add a new rule.

        Return the object to allow methods chaining.

        :param Rule rule: the rule to add to the repository
        :rtype: RulesDefinition
        """
        if rule in self:
            raise KeyError('rule already exists: {}'.format(rule))
        super(RulesDefinition, self).add(rule)
        return self

    def update(self, rules):
        """Update rules in the set.

        Return the object to allow methods chaining.

        :param collections.Iterable[Rule] rules: the list of rules to add to
            the repository
        :rtype: RulesDefinition
        """
        for rule in rules:
            self.add(rule)
        return self


class RulesDefinitionXmlWriter(object):
    """Generate rules definition XML files from :class:`RulesDefinition`."""

    @classmethod
    def write(cls, rules, stream=sys.stdout):
        """Write the rules definition to XML into `stream`.

        :param RulesDefinition rules: the rules definition
        :param file stream: the output stream
        """

        builder = lxml.builder.ElementMaker()
        dom = builder.rules()
        for rule in rules:
            dom.append(cls.__create_rule(rule, builder))
        stream.write(lxml.etree.tostring(
            dom, pretty_print=True, xml_declaration=True, encoding='UTF-8'))

    @classmethod
    def __create_rule(cls, rule, builder):
        """Create a single XML <rule> node.

        :param Rule rule: the rule to serialize
        :param lxml.builder.ElementMaker builder: the XML node builder
        :rtype: lxml.etree.Element
        """
        node = builder.rule(
            builder.key(rule.key),
            builder.name(rule.name),
            builder.description(rule.description)
        )
        if rule.severity is not None:
            node.append(builder.severity(rule.severity.name))
        if rule.type is not None:
            node.append(builder.type(rule.type.name))
        for tag in rule.tags:
            node.append(builder.tag(tag))
        return node


class RulesDefinitionXmlReader(object):
    """Load rules definition XML files into :class:`RulesDefinition`.

    :type log: logging.Logger
    """

    def __init__(self):
        self.log = logging.getLogger(self.__class__.__name__)

    def load(self, rules, filename):
        """Load definition of rules from an XML file.

        :param RulesDefinition rules: the rules repository in which to store
            the loaded rules
        :param str filename: the XML file containing the rules definition
        """
        dom = lxml.etree.parse(filename)
        for node in dom.findall('rule'):
            rules.add(self.__load_rule(node, dom))

    def __load_rule(self, node, dom):
        """Load a rule from an XML node.

        :param str node: the XML <rule> node
        :param lxml.etree.ElementTree dom: the XML dom root
        :rtype: Node
        """
        assert node.tag == 'rule', 'expected a <rule> node'

        payload = {
            'key': None,
            'name': None,
            'description': None,
            'type': None,
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
    """Generate rules profile XML files."""

    @staticmethod
    def write(language, name, rules_definitions, stream=sys.stdout):
        """Write the rules profile to XML into `stream`.

        :param str language: the language targeted by the rules profile
        :param str name: the name of the rules profile
        :param rules_definitions: the collection of :class:`RulesDefinition` to
            activate in the profile
        :type rules_definitions: collections.Iterable[RulesDefinition]
        :param file stream: the output stream
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
