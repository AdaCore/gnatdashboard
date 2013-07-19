import GPS
import os
import imp
import Qmt
import logging
import qmt_api.utils
from qmt_api.db import SessionFactory
from qmt_api.utils import OutputParser, create_parser, get_project_obj_dir

logging.basicConfig(format='[%(levelname)s] %(message)s', level=Qmt.log_level())
logger = logging.getLogger(__name__)

## tool_output ###############################################################
##
class tool_output ():
    """"Initate tool_output module

        This class is use by GPS in order to manage executed process output.
    """
    @staticmethod
    def create_parser (name, child=None):
        return create_parser (name, child)

## PluginRunner #############################################################
##
class PluginRunner(object):
    """Class that load python plugin

       Retreive all plugins: core and user
       Plugin execution order:
            - core plugin
            - user plugin
            - sonar plugin if active
    """

    SCRIPT_SUFFIX='.py'

    def __remove_plugin_to_deactivate(self, plugins):
        """Deactive plugin according to attribute Plugin_Off in the project
           file
        """
        for p in  qmt_api.utils.get_qmt_property_str('Plugin_Off'):
            try:
                plugins.remove(p)
                logger.debug('Plugin %s is deactivated' % p)
            except ValueError:
                logger.warn('Plugin %s will not be deactive' % p)
                logger.warn('Not in the plugin list to execute or not installed at all')

    def __move_sonar_to_end(self, plugins):
        try:
            sonar_plugin = plugins.pop(plugins.index('sonarrunner'))
            plugins.append(sonar_plugin)
        except ValueError:
            return

    def __get_plugins_from_dirs(self, plugins):
        files = os.listdir(Qmt.core_plugin_dir()) + os.listdir(Qmt.user_plugin_dir())
        for f in files:
            if f.endswith(self.SCRIPT_SUFFIX):
                plugins.append(f.replace(self.SCRIPT_SUFFIX, ''))

    def get_all_plugins(self):
        """Retrieve all plugin for qualimetrics

          If Plugin_Scheduling attribute is set in project file, the given list
          is used to order plugin in list,
          otherwise default order is applied
        """
        # Retrieve plugin list to execute
        plugins = qmt_api.utils.get_qmt_property_list('Plugin_Scheduling')
        if not plugins:
            self.__get_plugins_from_dirs(plugins)
        else:
            logger.debug('Plugin order execution from project file will be used')

        # Remove from the list plugin to deactive
        self.__remove_plugin_to_deactivate(plugins)
        self.__move_sonar_to_end(plugins)
        logger.debug('Plugin to execute: %s' % str(plugins))
        return plugins

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

    def __execute_plugin(self, plugin_instance):
        plugin_instance.setup()
        success = plugin_instance.execute()
        plugin_instance.teardown()
        logger.debug('Plugin returns: %s' % str(success))
        logger.info('..... %s  %s' % (plugin_instance.name, ('[OK]' if success else '[FAIL]')))

    def run_plugins(self, session_factory):
        # Fetch all plugins
        for module in self.get_all_plugins():
            plugin_class = self.get_plugin_class(module)
            if plugin_class:
                session = session_factory.get_session()
                # Instanciate the plugin
                plugin = plugin_class(session)
                logger.info('===== Running %s' % plugin.name)
                self.__execute_plugin(plugin)
                session.close()

def _entry_point():
    # Initialise
    runner = PluginRunner()
    # Execute
    sessionfactory = SessionFactory()
    runner.run_plugins(sessionfactory)

if __name__ == '__main__':
    _entry_point()

