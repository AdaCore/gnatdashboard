#########################################
#        Qualimetrics Basic  API        #
#########################################

import GPS
import Qmt
import os
import utils
import logging
from plugin import GPSTarget

logger = logging.getLogger(__name__)

## OutputParserMetaClass ######################################################
##
class OutputParserMetaClass(type):
    registered = dict()   # list of registered parsers

    def __new__(cls, name, bases, attrs):
        new_class = type.__new__(cls, name, bases, attrs)
        OutputParserMetaClass.registered[new_class.get_name()] = new_class
        return new_class

    def get_name(self):
        """Return the name of the parser, either from a "name" class
            attribute, or from the class name
        """
        return getattr(self, 'name', self.__name__).lower()

## OutputParser ##############################################################
##
class OutputParser(object):
    __metaclass__ = OutputParserMetaClass

    def __init__(self,child):
        self.child = child

    def on_stdout(self,text):
        if self.child != None:
            self.child.on_stdout (text)

    def on_stderr(self,text):
        if self.child != None:
           self.child.on_stderr (text)

    def on_exit(self,status=0):
        # Update Tool execution status
        logging.debug('Process exit status: %s' % status)
        if status != 0:
            GPSTarget.EXECUTION_SUCCESS=False
        else:
            GPSTarget.EXECUTION_SUCCESS=True

        if self.child != None:
            self.child.on_exit (status)

## create_parser ######################################################
##
def create_parser(name, child=None):
    if OutputParserMetaClass.registered.has_key (name):
        return OutputParserMetaClass.registered[name](child)
    else:
        return None

## get_project_obj_dir ######################################################
##
def get_project_obj_dir():
    return GPS.Project.root().object_dirs()[0]

## get_project_obj_dir ######################################################
##
def get_db_path():
    return os.path.join(get_project_obj_dir(), Qmt.db_relative_path())

## get_project_obj_dir #######################################################
##
def get_qmt_root_dir():
    return os.path.join(get_project_obj_dir(), Qmt.qmt_root_dir())

## get_project_name ##########################################################
##
def get_project_name():
    return GPS.Project.root().name()

## get_qmt_property_str #######################################################
##
def get_qmt_property_str(key):
    return GPS.Project.root().get_attribute_as_string(attribute=key, package='Qualimetrics')

## get_qmt_property_list ######################################################
##
def get_qmt_property_list(key):
    return GPS.Project.root().get_attribute_as_list(attribute=key, package='Qualimetrics')

