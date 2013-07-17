#########################################
#        Qualimetrics Plugin API        #
#########################################

import os
import utils
import Qmt
import random
import db
from abc import ABCMeta, abstractmethod

## Plugin ######################################################
##
class Plugin:
    __metaclass__ = ABCMeta
    # A clear mention need to be done in documentation about the log file
    # name for tool and the declaration of a custom output parser
    # in plugin script which is mandatory for the consistency of
    # tool execution information.
    LOG_FILE_NAME='qmt-tool-%d.log' % random.randint(1,100)

    def __init__(self, name):
        self.name = name

    @abstractmethod
    def setup(self):
        return

    @abstractmethod
    def execute(self):
        return

    @classmethod
    def get_log_file_path(cls):
        """Class method that returns full path for the plugin logging file"""
        path = os.path.join(utils.get_project_obj_dir(),
                            Qmt.logs_dir(), cls.LOG_FILE_NAME)
        return path

## Tool ######################################################
##
class Tool(Plugin):
    # Value updated by utils.OutputParser
    EXECUTION_SUCCES=None

    def __init__(self, name, session):
        super(Tool, self).__init__(name)
        self.session= session
        self.my_tool = db.Tool(self.name)
        self.session.add(self.my_tool)
        self.session.commit()

