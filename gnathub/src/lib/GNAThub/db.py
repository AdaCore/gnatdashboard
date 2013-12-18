##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                        Copyright (C) 2013, AdaCore                       ##
##                                                                          ##
## The QM is free software; you can redistribute it  and/or modify it       ##
## under terms of the GNU General Public License as published by the Free   ##
## Software Foundation; either version 3, or (at your option) any later     ##
## version.  The QM is distributed in the hope that it will be useful,      ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License  for more details. You  should  have  received a copy of the GNU ##
## General Public License  distributed with the QM; see file COPYING3. If   ##
## not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite ##
## 330, Boston, MA 02111-1307, USA.                                         ##
##                                                                          ##
##############################################################################

"""This module defines the various DAO manipulated by GNAThub.
"""

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import (Column, Integer, String, ForeignKey, Boolean, Table)
from sqlalchemy.orm import relationship


BASE = declarative_base()

RULE_KIND, METRIC_KIND = range(2)
PROJECT_KIND, DIRECTORY_KIND, FILE_KIND = range(3)

RESOURCE_MESSAGE_TABLE = \
    Table('resources_messages', BASE.metadata,
          Column('resource_id', Integer, ForeignKey('resources.id')),
          Column('message_id', Integer, ForeignKey('messages.id')))


class Tool(BASE):
    """A tool object, mapping a external tool, e.g. GNATcheck, GNATmetric..."""

    __tablename__ = 'tools'

    id = Column(Integer, primary_key=True)
    name = Column(String)

    rules = relationship("Rule", back_populates="tool")

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "<Tool('%s')>" % self.name


class Rule(BASE):
    """A Rule object, mapping a tool rule."""

    __tablename__ = 'rules'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    identifier = Column(String)
    kind = Column(Integer)
    tool_id = Column(Integer, ForeignKey('tools.id'))

    tool = relationship("Tool", back_populates='rules')
    messages = relationship("Message")

    def __init__(self, name, identifier, tool, kind):
        self.name = name
        self.identifier = identifier
        self.tool = tool
        self.kind = kind

    def __repr__(self):
        return "<Rule('%s', '%s', '%s')>" % (self.name, self.identifier,
                                             self.tool.name)


class Category(BASE):
    """A Category object, mapping a rule category."""

    __tablename__ = 'categories'

    id = Column(Integer, primary_key=True)
    label = Column(String)
    on_side = Column(Boolean)

    messages = relationship("Message")

    def __init__(self, label, on_side=False):
        self.label = label
        self.on_side = on_side

    def __repr__(self):
        return "<Category('%s')>" % (self.label)


class Message(BASE):
    """A Message object."""

    __tablename__ = 'messages'

    id = Column(Integer, primary_key=True)
    data = Column(String)
    rule_id = Column(Integer, ForeignKey('rules.id'))
    category_id = Column(Integer, ForeignKey('categories.id'))

    rule = relationship("Rule", back_populates='messages')
    category = relationship("Category", back_populates='messages')

    def __init__(self, data, rule, category=None):
        self.data = data
        self.rule = rule
        self.category = category

    def __repr__(self):
        return "<Message('%s', '%s')>" % (self.rule.name, self.data)


class Resource(BASE):
    """A Resource object, mapping a entity resource, i.e. a file."""

    __tablename__ = 'resources'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    kind = Column(Integer)

    lines = relationship("Line", back_populates='resource')
    messages = relationship("Message", secondary=RESOURCE_MESSAGE_TABLE,
                            backref="resources")

    def __init__(self, name, kind):
        self.name = name
        self.kind = kind

    def __repr__(self):
        return "<Resource('%s', '%s')>" % (self.name, self.kind)


class Line(BASE):
    """A line object, mapping a line from a resource."""

    __tablename__ = 'lines'

    id = Column(Integer, primary_key=True)
    line = Column(Integer)
    resource_id = Column(Integer, ForeignKey('resources.id'))
    messages = relationship("LineMessage", backref="line")
    resource = relationship("Resource", back_populates='lines')

    def __init__(self, line, resource):
        self.line = line
        self.resource = resource

    def __repr__(self):
        return "<Line('%s', '%s')>" % (self.line, self.resource)


# pylint: disable=W0232
# Disable class has no __init__ method
class LineMessage(BASE):
    """Join table between Line and Message, with the additional information of
    range.
    """

    __tablename__ = 'lines_messages'

    id = Column(Integer, primary_key=True)

    line_id = Column(Integer, ForeignKey('lines.id'))
    message_id = Column(Integer, ForeignKey('messages.id'))
    col_begin = Column(Integer)
    col_end = Column(Integer)
    message = relationship("Message")
