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
import GNAThub.project

import abc
import os

from GNAThub import Log
from GNAThub.db import SessionFactory
from GNAThub.utils import create_parser


class tool_output ():
    """"Initate tool_output module

        This class is use by GPS in order to manage executed process output.
    """

    @staticmethod
    def create_parser(name, child=None):
        return create_parser(name, child)


class PluginRunner(object):
    """Class that loads python plugins.

       Retrieve all plugins: core and user

       Plugin execution order:
            - core plugin
            - user plugin
            - sonar plugin if active
    """

    PLUGIN_SUFFIX = '.py'

    def __remove_plugin_to_deactivate(self, plugins):
        """Deactivate plugins according to attribute Plugin_Off in the project
        file.
        """

        for p in GNAThub.project.property_as_list('Plugin_Off'):
            if p in plugins:
                plugins.remove(p)
                Log.debug('Plugin %s is deactivated' % p)

            else:
                Log.warn('Plugin %s not deactivated' % p)
                Log.warn('Not in plugin list to execute or not installed')

    def __move_sonar_to_end(self, plugins):
        """Locates the sonarrunner plugin and delays its execution to the end.
        """

        SONARRUNNER = 'sonarrunner'

        if SONARRUNNER in plugins:
            plugins.remove(SONARRUNNER)
            plugins.append(SONARRUNNER)

    def __get_plugins_from_dirs(self, plugins):
        """Retrieves projet sepecific plugins specified in the project file."""

        files = os.listdir(GNAThub.core_plugins())
        files.extend(os.listdir(GNAThub.extra_plugins()))

        for f in files:
            if f.endswith(self.PLUGIN_SUFFIX):
                plugins.append(f.replace(self.PLUGIN_SUFFIX, ''))

    def __get_plugin_project_specific(self):
        specific_plugins = []

        for plugin in GNAThub.project.property_as_list('Specific_Plugins'):
            if os.path.exists(plugin):
                try:
                    plugin_golbals = {}
                    execfile(plugin, plugin_golbals)
                    specific_plugins.append(plugin_golbals)
                    Log.debug('Load specific plugin file: %s' % plugin)

                except IOError:
                    Log.warn('Unable to load plugin: %s, ignoring...' % plugin)
                    Log.info('')

            else:
                Log.warn('Ignoring plugin because not found: %s' % plugin)
                Log.info('')

        return specific_plugins

    def get_all_plugins(self):
        """Retrieves all plugins for GNAThub

          If Plugin_Scheduling attribute is set in project file, the given list
          is used to order plugin in list,
          otherwise default order is applied
        """

        plugins = GNAThub.plugins()

        if len(plugins):
            plugins = [plugin.strip() for plugin in plugins.split(',')]
        else:
            plugins = GNAThub.project.property_as_list('Plugins')

            # GPS CLI returns: a empty list if property is not mentionned
            if not plugins:
                self.__get_plugins_from_dirs(plugins)
            else:
                Log.debug('Project file execution order will be used')

        plugins = plugins + self.__get_plugin_project_specific()

        # Remove from the list plugin to deactive
        self.__remove_plugin_to_deactivate(plugins)
        self.__move_sonar_to_end(plugins)

        return filter(lambda x: x is not None and x != '', plugins)

    def get_plugin_class(self, plugin):
        try:
            # If not a project specific plugin
            if not isinstance(plugin, dict):
                module = __import__(plugin, fromlist=['*'])

                for key in module.__dict__:
                    if key.lower() == plugin.lower():
                        return getattr(module, key)

            else:
                # For project specific plugins
                for key in plugin:
                    if type(plugin[key]) is abc.ABCMeta:
                        if plugin[key].__base__ is GNAThub.Plugin:
                            return plugin[key]

                Log.warn('No class extending GNAThub.Plugin found')

        except (ImportError,  AttributeError, ValueError) as e:
            Log.fatal('')
            Log.fatal('Failed to load plugin: %s' % plugin)
            Log.fatal('Caused by: %s' % e)
            Log.fatal('')

    def __execute_plugin(self, plugin):
        """Call method setup, execute and teardown for a plugin instance.
           Manage final ouput for the the plugin, if succed or not.

           Parameter:
               - plugin_instance: instance of the plugin to execute
        """

        plugin.setup()
        exit_code = plugin.execute()
        plugin.teardown()

        Log.debug('%s exit code: %d' % (plugin.name, exit_code))

        if exit_code == GNAThub.EXEC_SUCCESS:
            Log.debug('%s execution succeeded' % plugin.name)
        else:
            Log.info('%s execution failed' % plugin.name)

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
                plugin = plugin_class(session)
                Log.debug('%s: running plugin' % plugin.name)
                Log.info(' '.join(plugin.display_command_line()))
                self.__execute_plugin(plugin)
                session.close()


# Script entry point
if __name__ == '__main__':
    PluginRunner().run_plugins(SessionFactory())
