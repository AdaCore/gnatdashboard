#########################################
#        Qualimetrics Plugin API        #
#########################################

import os
import utils
import Qmt
from dao import DAO
from abc import ABCMeta, abstractmethod

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
    def __init__(self, name, dao):
        super(Tool, self).__init__(name)
        self.dao = dao
        self.dao.save_tool(name)

