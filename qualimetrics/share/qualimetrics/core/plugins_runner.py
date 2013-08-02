import abc
import imp
import sys
import GPS
import os
import Qmt
import logging
import qmt_api.utils
from qmt_api import plugin
from qmt_api.db import SessionFactory
from qmt_api.plugin import Plugin
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

## PluginRunner ###############################################################
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
        """Retrieve projet sepecific plugin, form project file file """
        files = os.listdir(Qmt.core_plugin_dir()) + os.listdir(Qmt.user_plugin_dir())

        for f in files:
            if f.endswith(self.SCRIPT_SUFFIX):
                plugins.append(f.replace(self.SCRIPT_SUFFIX, ''))

    def __get_plugin_project_specific(self):
        specific_plugins = []

        for plugin in qmt_api.utils.get_qmt_property_list('Specific_Plugins'):
            if os.path.exists(plugin):
                try:
                    plugin_golbals = {}
                    execfile(plugin, plugin_golbals)
                    specific_plugins.append(plugin_golbals)
                    logger.debug('Load specific plugin file: %s' % plugin)
                except IOError:
                    logger.warn('Ignoring plugin because unable to load file: %s' % plugin)
                    logger.info('')
            else:
                logger.warn('Ignoring plugin because not found: %s' % plugin)
                logger.info('')

        return specific_plugins

    def get_all_plugins(self):
        """Retrieve all plugin for qualimetrics

          If Plugin_Scheduling attribute is set in project file, the given list
          is used to order plugin in list,
          otherwise default order is applied
        """
        plugins = qmt_api.utils.get_qmt_property_list('Plugin_Scheduling')

        # GPS CLI returns a list with an empty string if property is not
        # mentionned
        if plugins == ['']:
            plugins.remove('')
            self.__get_plugins_from_dirs(plugins)
        else:
            logger.debug('Plugin order execution from project file will be used')

        plugins = plugins + self.__get_plugin_project_specific()

        # Remove from the list plugin to deactive
        self.__remove_plugin_to_deactivate(plugins)
        self.__move_sonar_to_end(plugins)

        return plugins

    def get_plugin_class(self, plugin):
        try:
            # If not a project specific plugin
            if not isinstance(plugin, dict):
                module = __import__(plugin, fromlist=['*'])
                return getattr(module, plugin.capitalize())
            else:
                # For project spefcific plugins
                for key in plugin:
                    if type(plugin[key]) is abc.ABCMeta:
                        if plugin[key].__base__ is Plugin:
                            return plugin[key]
                logger.warn('No class extending Plugin object was found for project specific plugin')
        except ImportError as e:
            logger.error('')
            logger.error('/!\  Unable to load plugin: %s  /!\\' % plugin)
            logger.error('Error: %s' % str(e))
            logger.error('')
        except AttributeError:
            logger.error('')
            logger.error('/!\  Cannot load plugin class %s  /!\\' % plugin)
            logger.error('Should implement a class named %s' % (plugin.capitalize()))
            logger.error('')

    def __execute_plugin(self, plugin_instance):
        """Call method setup, execute and teardown for a plugin instance.
           Manage final ouput for the the plugin, if succed or not.

           Parameter:
               - plugin_instance: instance of the plugin to execute
        """
        plugin_instance.setup()
        success = plugin_instance.execute()
        plugin_instance.teardown()

        logger.debug('Plugin execution returns: %s' % str(success))
        success_msg = ('[OK]' if success == plugin.EXEC_SUCCESS
                              else '[FAIL]')

        logger.info('..... %s  %s' % (plugin_instance.name, success_msg))
        logger.info('')

    def run_plugins(self, session_factory):
        """Fetch all python plugins: core, user, project specific; retrieve the
           plugin class and instanciate it.
           Manage creation and destruction of database session for the plugins.

           Parameter:
               - session_factory: used to create database session
        """
        # Fetch all plugins
        for p in self.get_all_plugins():
            plugin_class = self.get_plugin_class(p)

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

