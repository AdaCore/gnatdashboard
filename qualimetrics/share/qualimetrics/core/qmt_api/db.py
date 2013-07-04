import utils
from qmt_api import Session
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine, Column, Integer, String, ForeignKey
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

    def __init__(self, name, identifier, tool, kind=1):
        self.name = name
        self.identifier = identifier
        self.tool = tool
        self.kind = kind

    def __repr__(self):
        return "<Rule('%s', '%s', '%s')>" % (self.name, self.identifier,
        self.tool.name)

