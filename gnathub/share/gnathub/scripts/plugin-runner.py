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

# pylint: disable=C0103
# Disable "Invalid name {}"

"""Scans for GNAThub-specific plug-ins and registers their execution before
spawning the main event loop.
"""

import GNAThub
import GNAThub.project

import abc
import os

from GNAThub import Log


class PluginRunner(object):
    """Class that loads python plugins.

       Retrieve all plugins: core and user

       Plugin execution order:
            - core plugin
            - user plugin
            - sonar plugin if active
    """

    PLUGIN_SUFFIX = '.py'
    SONARRUNNER_PLUGIN = 'sonarrunner'

    def __remove_plugin_to_deactivate(self, plugins):
        """Deactivate plugins according to attribute Plugin_Off in the project
        file.
        """

        for plugin in GNAThub.project.property_as_list('Plugin_Off'):
            if plugin in plugins:
                plugins.remove(plugin)
                Log.debug('Plugin %s is deactivated' % plugin)

            else:
                Log.warn('Plugin %s not deactivated' % plugin)
                Log.warn('Not in plugin list to execute or not installed')

    def __move_sonar_to_end(self, plugins):
        """Locates the sonarrunner plugin and delays its execution to the end.
        """

        if self.SONARRUNNER_PLUGIN in plugins:
            plugins.remove(self.SONARRUNNER_PLUGIN)
            plugins.append(self.SONARRUNNER_PLUGIN)

    def __get_plugins_from_dirs(self, plugins):
        """Retrieves projet sepecific plugins specified in the project file."""

        files = os.listdir(GNAThub.core_plugins())
        files.extend(os.listdir(GNAThub.extra_plugins()))

        for filename in files:
            if filename.endswith(self.PLUGIN_SUFFIX) \
                    and not filename.startswith('_'):
                plugins.append(filename.replace(self.PLUGIN_SUFFIX, ''))

    def __get_plugin_project_specific(self):
        """Returns the project-specific plug-ins list if defined in the project
        file.

        RETURNS
            :rtype: a list of strings.
        """

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

            else:
                Log.warn('Ignoring plugin because not found: %s' % plugin)
                Log.info('')

        return specific_plugins

    def __get_all_plugins(self):
        """Retrieves all plugins for GNAThub

          If Plugin_Scheduling attribute is set in project file, the given list
          is used to order plugin in list,
          otherwise default order is applied
        """

        # pylint: disable=E1111
        # Disable "Assigning to function call which doesn't return"
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

        return [x for x in plugins if x is not None and x != '']

    def __get_plugin_class(self, plugin):
        """Returns a plugin class object if such a class with the given name
        exists.

        PARAMETERS
            :param plugin: the name of the plugin to find.
            :type plugin: a string.

        RETURNS
            :rtype: a Python class object.
        """

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

        except (ImportError,  AttributeError, ValueError) as ex:
            Log.fatal('')
            Log.fatal('Failed to load plugin: %s' % plugin)
            Log.fatal('Caused by: %s' % ex)
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

    def run_plugins(self):
        """Fetch all python plugins: core, user, project specific; retrieve the
           plugin class and instanciate it.
           Manage creation and destruction of database session for the plugins.

           Parameter:
               - session_factory: used to create database session
        """

        # Fetch all plugins
        for plugin in self.__get_all_plugins():
            plugin_class = self.__get_plugin_class(plugin)

            if plugin_class:
                GNAThub.register(plugin_class())

        GNAThub.run()


# Script entry point
if __name__ == '__main__':
    PluginRunner().run_plugins()
