#########################################
#        Qualimetrics Plugin API        #
#########################################

import os
import utils
import Qmt
from abc import ABCMeta, abstractmethod
import db

## Plugin ######################################################
##
class Plugin:
    __metaclass__ = ABCMeta

    def __init__(self, name):
        self.name = name

    @abstractmethod
    def setup(self):
        return

    @abstractmethod
    def execute(self):
        return

    @classmethod
    def get_log_file_path(cls, name):
        """Class method that returns full path for the plugin logging file

           Parameters:
            - name: name of the file, without the extension
        """
        filename = name + '.log'
        path = os.path.join(utils.get_project_obj_dir(),
                            Qmt.logs_dir(), filename)
        return path

## Tool ######################################################
##
class Tool(Plugin):
    def __init__(self, name, session):
        super(Tool, self).__init__(name)
        self.session= session
        self.my_tool = db.Tool(self.name)
        self.session.add(self.my_tool)
        self.session.commit()

