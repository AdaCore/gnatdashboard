import os
import imp
from qmt_api.utils import OutputParser, create_parser


## tool_output ###############################################################
##
class tool_output ():
    """"Imitate tool_output module"""
    @staticmethod
    def create_parser (name, child=None):
        return create_parser (name, child)

## PluginExecutor #############################################################
##
class PluginExecutor(object):
    SCRIPT_SUFFIX='.py'

    def __init__(self):
        self.base_dir = os.path.join('share', 'qualimetrics')
        self.core_dir = os.path.join(os.getcwd(), self.base_dir, 'core', 'plug-ins')
        self.user_dir = os.path.join(os.getcwd(), self.base_dir, 'plug-ins')

    def get_all_plugins(self):
        files = os.listdir(self.core_dir) + os.listdir(self.user_dir)
        module_list = [f.replace(self.SCRIPT_SUFFIX, '') for f in files
                       if f.endswith(self.SCRIPT_SUFFIX)]
        return module_list

    def get_plugin_class(self, module_name):
        try:
            module = __import__(module_name, fromlist=['*'])
            plugin_class = getattr(module, module_name.capitalize())
            print plugin_class
            return plugin_class
        # except ImportError:
        #    print 'Unable to load plugin: %s' % f
        except AttributeError:
            print 'Cannot execute plugin %s' % module_name
            print 'Should implement a class named %s' % (module_name.capitalized)

    def execute_plugins(self):
        for module in self.get_all_plugins():
            print 'Loading module: %s' % module
            plugin_class = self.get_plugin_class(module)
            plugin = plugin_class()
            plugin.setup()
            plugin.execute()

def _entry_point():
    executor = PluginExecutor()
    executor.execute_plugins()

if __name__ == '__main__':
    _entry_point()
