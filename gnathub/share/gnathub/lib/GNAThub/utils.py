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

import GNAThub
import GPS
import os

from GNAThub import GPSTarget, Log


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
        Log.debug('Process exit status: %s' % status)

        if status != 0:
            GPSTarget.EXECUTION_SUCCESS = GNAThub.EXEC_FAIL
        else:
            GPSTarget.EXECUTION_SUCCESS = GNAThub.EXEC_SUCCESS

        if self.child != None:
            self.child.on_exit (status)


def create_parser(name, child=None):
    if OutputParserMetaClass.registered.has_key (name):
        return OutputParserMetaClass.registered[name](child)
    else:
        return None


def get_project_obj_dir():
    return GPS.Project.root().object_dirs()[0]


def get_db_path():
    return os.path.join(get_project_obj_dir(), GNAThub.database())


def get_qmt_root_dir():
    return os.path.join(get_project_obj_dir(), GNAThub.root())


def get_project_name():
    return GPS.Project.root().name()


def get_qmt_property_str(key):
    return GPS.Project.root().get_attribute_as_string(attribute=key,
                                                      package='GNAThub')


def get_qmt_property_list(key):
    return GPS.Project.root().get_attribute_as_list(attribute=key,
                                                    package='GNAThub')

