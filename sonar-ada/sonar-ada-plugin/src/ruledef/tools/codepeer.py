# Sonar Ada Plugin (GNATdashboard)
# Copyright (C) 2016-2017, AdaCore
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

import docutils
import docutils.frontend
import docutils.nodes
import docutils.parsers.rst
import docutils.utils

from enum import Enum
from gnathub import encoding
from sonarqube.rule import Rule, RulesDefinition, Type


# Rule category as defined by CodePeer manual
Category = Enum(
    'Category', 'CHECK WARNING RACE_CONDITION INFORMATIONAL SUPPRESSED')


class CodePeerRulesDefinition(RulesDefinition):
    """Define CodePeer coding rules."""

    # Repository unique identifier
    REPOSITORY_KEY = 'codepeer'

    def __init__(self):
        """Create an empty rule repository.

        To create a populated repository, use :meth:`create_from_doc`.
        """
        super(CodePeerRulesDefinition, self).__init__(self.REPOSITORY_KEY)

    @classmethod
    def create_from_doc(cls, messages_and_annotations_rst):
        """Create a :class:`CodePeerRulesDefinition` from documentation.

        Use CodePeer documentation file :file:`messages_and_annotations.rst` to
        populate the rule repository.

        :param str messages_and_annotations_rst: path to the CodePeer
            documentation file :file:`messages_and_annotations.rst`
        :rtype: CodePeerRulesDefinition
        """

        settings = docutils.frontend.OptionParser(
            components=(docutils.parsers.rst.Parser,),
            defaults={'report_level': 'quiet'}
        ).get_default_values()
        chapter_name = 'Messages and Annotations'

        # Create a new empty `docutils.nodes.document` tree
        document = docutils.utils.new_document(chapter_name, settings)

        # Load the whole reST document
        with open(messages_and_annotations_rst, 'r') as fdin:
            messages_and_annotations = fdin.read()

        # Create a reST parser and parse the document
        parser = docutils.parsers.rst.Parser()
        parser.parse(messages_and_annotations, document)

        # Walk the document to collect the rules
        collector = _CollectRulesVisitor(document)
        document.walk(collector)

        # Create the CodePeer rules from collected data
        rules = CodePeerRulesDefinition()
        for kind, cat in [(collector.checks, Category.CHECK),
                          (collector.warnings, Category.WARNING),
                          (collector.race_conditions, Category.RACE_CONDITION),
                          (collector.informations, Category.INFORMATIONAL)]:
            for msg, desc in kind:
                rules.add(cls.__create_rule(msg, desc, cat))
        return rules

    @classmethod
    def __create_rule(cls, message, description, category):
        """Create a new rule.

        Encode the rule key using the message and the category. Attach the
        description to the rule.

        :param str message: the rule message
        :param str description: the rule description
        :param Category category: the message category (as defined by CodePeer)
        :rtype: Rule
        """

        if category == Category.RACE_CONDITION:
            type = Type.BUG
        elif category in (Category.CHECK, Category.WARNING):
            type = Type.CODE_SMELL
        else:
            type = None

        kwargs = {
            'description': description,
            'type': type,
            'tags': ('codepeer', category.name.lower().replace('_', '-'))
        }
        return Rule(encoding.encode_codepeer_key(message), message, **kwargs)


class _CollectRulesVisitor(docutils.nodes.SparseNodeVisitor):
    """Walk the reST document and collect CodePeer rules.

    CodePeer rules are separated in three categories:
      * Checks (runtime checks, user checks, validity checks)
      * Warnings (logic errors)
      * Race conditions

    :type checks: collections.Iterable[(str, str)]
    :type warnings: collections.Iterable[(str, str)]
    :type race_conditions: collections.Iterable[(str, str)]
    :type informations: collections.Iterable[(str, str)]
    """

    CHECK_SECTIONS = ('run-time checks', 'user checks',
                      'uninitialized and invalid variables')
    WARNING_SECTIONS = ('warning messages',)
    RACE_CONDITION_SECTIONS = ('race condition messages',)
    INFORMATION_SECTIONS = ('information messages',)

    def __init__(self, document):
        docutils.nodes.SparseNodeVisitor.__init__(self, document)

        # Use lists rather than sets to defer the unicity check to the
        # underlying `set` in `RulesDefinition`, thus avoiding duplicate
        # defensive code or silent discard of rules.
        self.checks = []
        self.warnings = []
        self.race_conditions = []
        self.informations = []

    def visit_section(self, node):
        # A `section` node has a `names` attributes which is a single-element
        # list.
        assert 'names' in node.attributes and len(node.attributes['names'])
        name = node.attributes['names'][0]

        # If the section is known to have rules declaration, create the
        # appropriate visitor for it.
        if name in self.CHECK_SECTIONS:
            pcv = _WalkTableVisitor(self.checks, self.document)
        elif name in self.WARNING_SECTIONS:
            pcv = _WalkTableVisitor(self.warnings, self.document)
        elif name in self.RACE_CONDITION_SECTIONS:
            pcv = _WalkTableVisitor(self.race_conditions, self.document)
        elif name in self.INFORMATION_SECTIONS:
            pcv = _WalkTableVisitor(self.informations, self.document)
        else:
            pcv = None

        # If a visitor was created, use it to walk the subtree
        if pcv is not None:
            node.walk(pcv)


class _WalkTableVisitor(docutils.nodes.SparseNodeVisitor):
    """Traverse a table and populate rules.

    This visitor is meant to walk a subtree of the document. It looks for
    tables and parses them to collect the rules.

    :type rules: collections.Iterable[(str, str)]
    """

    def __init__(self, rules, document):
        docutils.nodes.SparseNodeVisitor.__init__(self, document)
        self.rules = rules
        self.title_row_visited = False

    def visit_row(self, node):
        assert len(node.children) == 2, 'expected table w/ strictly 2 columns'

        # Extract cell content and normalize whitespaces
        message, description = node.children
        message = ' '.join(message.astext().split())
        description = ' '.join(description.astext().split())

        if not self.title_row_visited:
            # The first row of a table contains the titles; skip it
            assert message == 'Message' and description == 'Description'
            self.title_row_visited = True
        else:
            # Append the (message, description) pair to the provided rules set
            self.rules.append((message, description))
