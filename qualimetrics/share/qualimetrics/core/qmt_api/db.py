import utils
from qmt_api import Session
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import (create_engine, Column, Integer, String,
                        ForeignKey, Boolean, Table)
from sqlalchemy.orm import relationship, backref

Base = declarative_base()

## SessionFactory #############################################################
##
class SessionFactory(object):

    def __init__(self):
        self.__engine = create_engine('sqlite:///%s' % utils.get_db_path(),
                        echo=False)
        Session.configure(bind=self.__engine)

    def get_session(self):
        return Session ()

## Tool ######################################################################
##
class Tool(Base):
    __tablename__ = 'tools'

    id = Column(Integer, primary_key=True)
    name = Column(String)

    rules = relationship("Rule", back_populates="tool")

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "<Tool('%s')>" % self.name

## Rule #######################################################################
##
class Rule(Base):
    __tablename__ = 'rules'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    identifier = Column(String)
    kind = Column(Integer)
    tool_id = Column(Integer, ForeignKey('tools.id'))

    tool = relationship("Tool", back_populates='rules')
    messages = relationship("Message")

    def __init__(self, name, identifier, tool, kind=1):
        self.name = name
        self.identifier = identifier
        self.tool = tool
        self.kind = kind

    def __repr__(self):
        return "<Rule('%s', '%s', '%s')>" % (self.name, self.identifier,
        self.tool.name)

## Category ####################################################################
##
class Category(Base):
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

## resource_messages ########################################
##
resource_message_table = Table('resources_messages', Base.metadata,
        Column('resource_id', Integer, ForeignKey('resources.id')),
        Column('message_id', Integer, ForeignKey('messages.id'))
        )

## Message ####################################################################
##
class Message(Base):
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

## Resource ###################################################################
##
class Resource(Base):
    __tablename__ = 'resources'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    kind = Column(Integer)

    lines = relationship("Line", back_populates='resource')
    messages = relationship("Message", secondary=resource_message_table,
    backref="resources")

    def __init__(self, name, kind):
        self.name = name
        self.kind = kind

    def __repr__(self):
        return "<Resource('%s', '%s')>" % (self.name, self.kind)

## Line #######################################################################
##
class Line(Base):
    __tablename__ = 'lines'

    id = Column(Integer, primary_key=True)
    line = Column(Integer)
    resource_id = Column(Integer, ForeignKey('resources.id'))

    resource = relationship("Resource", back_populates='lines')

    def __init__(self, line, resource):
        self.line = line
        self.resource = resource

    def __repr__(self):
        return "<Line('%s', '%s')>" % (self.line, self.resource)

## LineMessage ################################################################
##
class LineMessage(Base):
    __tablename__ = 'lines_messages'
    id = Column(Integer, primary_key=True)
    line_id = Column(Integer, ForeignKey('lines.id'))
    message_id = Column(Integer, ForeignKey('messages.id'))
    col_begin = Column(Integer)
    col_end = Column(Integer)
