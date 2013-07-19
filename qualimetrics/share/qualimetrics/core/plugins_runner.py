import GPS
import os
import imp
import Qmt
import logging
from qmt_api.db import SessionFactory
from qmt_api.utils import OutputParser, create_parser, get_project_obj_dir

logging.basicConfig(format='[%(levelname)s] %(message)s', level=Qmt.log_level())
logger = logging.getLogger(__name__)

## tool_output ###############################################################
##
class tool_output ():
    """"Initate tool_output module"""
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
            logger.error('Unable to load plugin: %s' % module_name)
            logger.error(str(e))
            logger.info('.....  [FAIL]')
        except AttributeError:
            logger.error('Cannot load plugin class %s' % module_name)
            logger.error('Should implement a class named %s' % (module_name.capitalize()))
            logger.info('.....  [FAIL]')

    def run_plugins(self, session_factory):
        # Fetch all plugins
        for module in self.get_all_plugins():
            plugin_class = self.get_plugin_class(module)
            if plugin_class:
                session = session_factory.get_session()
                # Instanciate the plugin
                plugin = plugin_class(session)
                logger.info('===== Running %s' % plugin.name)
                plugin.setup()
                success = plugin.execute()
                plugin.teardown()
                logger.debug('Plugin returns: %s' % str(success))
                logger.info('..... %s  %s' % (plugin.name, ('[OK]' if success else '[FAIL]')))
                session.close()

def _entry_point():
    # Initialise
    runner = PluginRunner()
    # Execute
    sessionfactory = SessionFactory()
    runner.run_plugins(sessionfactory)

if __name__ == '__main__':
    _entry_point()

