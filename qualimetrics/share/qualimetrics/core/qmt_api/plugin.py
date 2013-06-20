#########################################
#        Qualimetrics Plugin API        #
#########################################

import os
import dao
import utils
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
        return os.path.join(utils.get_log_dir, filename)

## Tool ######################################################
##
class Tool(Plugin):
    def __init__(self, name):
        super(Tool, self).__init__(name)
        dao.save_tool(name)

