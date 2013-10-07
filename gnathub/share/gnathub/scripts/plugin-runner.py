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

import abc
import imp
import os
import sys

from GNAThub import Log
from GNAThub.db import SessionFactory
from GNAThub.utils import OutputParser, create_parser, get_project_obj_dir


class tool_output ():
    """"Initate tool_output module

        This class is use by GPS in order to manage executed process output.
    """

    @staticmethod
    def create_parser(name, child=None):
        return create_parser(name, child)


class PluginRunner(object):
    """Class that load python plugin

       Retrieve all plugins: core and user
       Plugin execution order:
            - core plugin
            - user plugin
            - sonar plugin if active
    """

    SCRIPT_SUFFIX = '.py'

    def __remove_plugin_to_deactivate(self, plugins):
        """Deactive plugin according to attribute Plugin_Off in the project
           file
        """
        for p in GNAThub.utils.get_qmt_property_list('Plugin_Off'):
            try:
                plugins.remove(p)
                Log.debug('Plugin %s is deactivated' % p)

            except ValueError:
                Log.warn('Plugin %s will not be deactive' % p)
                Log.warn('Not in the plugin list to execute or not installed at all')

    def __move_sonar_to_end(self, plugins):
        try:
            sonar_plugin = plugins.pop(plugins.index('sonarrunner'))
            plugins.append(sonar_plugin)

        except ValueError:
            return

    def __get_plugins_from_dirs(self, plugins):
        """Retrieve projet sepecific plugin, form project file file """
        files = os.listdir(GNAThub.core_plugins()) + os.listdir(GNAThub.extra_plugins())

        for f in files:
            if f.endswith(self.SCRIPT_SUFFIX):
                plugins.append(f.replace(self.SCRIPT_SUFFIX, ''))

    def __get_plugin_project_specific(self):
        specific_plugins = []

        for plugin in GNAThub.utils.get_qmt_property_list('Specific_Plugins'):
            if os.path.exists(plugin):
                try:
                    plugin_golbals = {}
                    execfile(plugin, plugin_golbals)
                    specific_plugins.append(plugin_golbals)
                    Log.debug('Load specific plugin file: %s' % plugin)

                except IOError:
                    Log.warn('Ignoring plugin because unable to load file: %s' % plugin)
                    Log.info('')

            else:
                Log.warn('Ignoring plugin because not found: %s' % plugin)
                Log.info('')

        return specific_plugins

    def get_all_plugins(self):
        """Retrieve all plugins for GNAThub

          If Plugin_Scheduling attribute is set in project file, the given list
          is used to order plugin in list,
          otherwise default order is applied
        """

        plugins = GNAThub.plugins()

        if len(plugins):
            plugins = [plugin.strip() for plugin in plugins.split(',')]
        else:
            plugins = GNAThub.utils.get_qmt_property_list('Plugins')

            # GPS CLI returns: a empty list if property is not mentionned
            if not plugins:
                self.__get_plugins_from_dirs(plugins)
            else:
                Log.debug('Plugin order execution from project file will be used')

        plugins = plugins + self.__get_plugin_project_specific()

        # Remove from the list plugin to deactive
        self.__remove_plugin_to_deactivate(plugins)
        self.__move_sonar_to_end(plugins)

        def is_not_empty(x): return x is not None and x != ''

        return filter(is_not_empty, plugins)

    def get_plugin_class(self, plugin):
        try:
            # If not a project specific plugin
            if not isinstance(plugin, dict):
                module = __import__(plugin, fromlist=['*'])
                return getattr(module, plugin.capitalize())
            else:
                # For project specific plugins
                for key in plugin:
                    if type(plugin[key]) is abc.ABCMeta:
                        if plugin[key].__base__ is GNAThub.Plugin:
                            return plugin[key]
                Log.warn('No class extending Plugin object was found for project specific plugin')
        except (ImportError,  AttributeError, ValueError) as e:
            Log.fatal('')
            Log.fatal('Failed to load plugin: %s' % plugin)
            Log.fatal('Caused by: %s' % e)
            Log.fatal('')

    def __execute_plugin(self, plugin_instance):
        """Call method setup, execute and teardown for a plugin instance.
           Manage final ouput for the the plugin, if succed or not.

           Parameter:
               - plugin_instance: instance of the plugin to execute
        """
        plugin_instance.setup()
        success = plugin_instance.execute()
        plugin_instance.teardown()

        Log.debug('%s exit code: %s' % (plugin_instance.name, str(success)))

        if success == GNAThub.EXEC_SUCCESS:
            Log.debug('%s execution succeeded' % plugin_instance.name)
        else:
            Log.fatal('%s execution failed' % plugin_instance.name)

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
                Log.info('Running module: %s' % plugin.name)
                self.__execute_plugin(plugin)
                session.close()


# Script entry point
if __name__ == '__main__':
    PluginRunner().run_plugins(SessionFactory())
