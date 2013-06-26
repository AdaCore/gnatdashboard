import GPS
import os
import imp
import Qmt
import sqlite3
from qmt_api.dao import DAO
from qmt_api.utils import OutputParser, create_parser, get_project_obj_dir

## tool_output ###############################################################
##
class tool_output ():
    """"Imitate tool_output module"""
    @staticmethod
    def create_parser (name, child=None):
        return create_parser (name, child)

## PluginRunner #############################################################
##
class PluginRunner(object):
    SCRIPT_SUFFIX='.py'

    def get_all_plugins(self):
        files = os.listdir(Qmt.core_plugin_dir()) + os.listdir(Qmt.user_plugin_dir())
        module_list = [f.replace(self.SCRIPT_SUFFIX, '') for f in files
                       if f.endswith(self.SCRIPT_SUFFIX)]
        return module_list

    def get_plugin_class(self, module_name):
        try:
            module = __import__(module_name, fromlist=['*'])
            plugin_class = getattr(module, module_name.capitalize())
            return plugin_class
        except ImportError as e:
            print 'Unable to load plugin: %s' % module_name
            print e
        except AttributeError:
            print 'Cannot execute plugin %s' % module_name
            print 'Should implement a class named %s' % (module_name.capitalized)

    def run_plugins(self, db_connection):
        for module in self.get_all_plugins():
            dao = DAO(db_connection)
            plugin_class = self.get_plugin_class(module)
            if not plugin_class:
                continue
            plugin = plugin_class(dao)
            plugin.setup()
            plugin.execute()

def _entry_point():
    # Initialise
    db_path = os.path.join(get_project_obj_dir(), Qmt.db_file_name())
    connection = sqlite3.connect(db_path)
    runner = PluginRunner()
    # Execute
    runner.run_plugins(connection)
    # Finish
    connection.close()

if __name__ == '__main__':
    _entry_point()
