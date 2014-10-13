############################################################################
#                                                                          #
#                               G N A T h u b                              #
#                                                                          #
#                     Copyright (C) 2013-2014, AdaCore                     #
#                                                                          #
# This is free software;  you can redistribute it  and/or modify it  under #
# terms of the  GNU General Public License as published  by the Free Soft- #
# ware  Foundation;  either version 3,  or (at your option) any later ver- #
# sion.  This software is distributed in the hope  that it will be useful, #
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License for  more details.  You should have  received  a copy of the GNU #
# General  Public  License  distributed  with  this  software;   see  file #
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy #
# of the license.                                                          #
#                                                                          #
############################################################################

"""Scans for GNAThub-specific plug-ins and registers their execution before
spawning the main event loop.
"""

import inspect
import logging
import os
import sys
import time

import GNAThub
from GNAThub import Console


# Create this script logger
MODULE = 'plugin-runner'
LOG = logging.getLogger(MODULE)


class GNAThubLoggingHandler(logging.Handler):
    """Custom logging handler that uses :class:`GNAThub.Logger` as back-end"""

    LOGGING_FUNCTIONS = {
        'DEBUG': GNAThub.Logger.debug,
        'INFO': GNAThub.Logger.info,
        'WARNING': GNAThub.Logger.warn,
        'ERROR': GNAThub.Logger.error,
        'CRITICAL': GNAThub.Logger.fatal
    }

    def __init__(self):
        super(GNAThubLoggingHandler, self).__init__()

        # Instances of GNAThub.Logger
        # This is used to select the correct logger to log with.
        self.loggers = {}

    def emit(self, record):
        """Inherited."""

        if record.name in self.loggers:
            logger = self.loggers[record.name]
        else:
            logger = GNAThub.Logger(record.name)
            self.loggers[record.name] = logger

        try:
            message = self.format(record)

            method = GNAThubLoggingHandler.LOGGING_FUNCTIONS[record.levelname]
            method.__get__(logger)(message)

        except (KeyboardInterrupt, SystemExit):
            raise

        except:
            self.handleError(record)
            return


class PluginRunner(object):
    """Class that loads python plugins

    Retrieve all plugins: core and user

    Plugin execution order:
      * core plugin
      * user plugin
      * sonar plugin (if active)
    """

    PLUGIN_EXT = '.py'
    SONAR_RUNNER = 'sonar-runner'

    def __init__(self):
        # The list of plugins to be sequentially executed
        self.plugins = PluginRunner.auto_discover_plugins()

    @staticmethod
    def info(message):
        """Displays an informative message, prefixed with the plug-in name

        :param message: the message to log
        :type message: str
        """

        Console.info(message, prefix=MODULE)

    @staticmethod
    def warn(message):
        """Displays a warning message, prefixed with the plug-in name

        :param message: the message to log
        :type message: str
        """

        Console.warn(message, prefix=MODULE)

    @staticmethod
    def error(message):
        """Displays an error message, prefixed with the plug-in name

        :param message: the message to log
        :type message: str
        """

        Console.error(message, prefix=MODULE)

    @staticmethod
    def schedule(plugins):
        """Schedules the plugins execution order

        Some system plugins might need to be executed in a specific order, or
        for example, for the Sonar Runner plugin, last. This routine takes care
        of ordering the plugins in their final execution order.

        :param plugins: collection of plugins to be executed
        :type plugins: list[GNAThub.Plugin]
        :return: the ordered list of plugins
        :rtype: list[GNAThub.Plugin]
        """

        def plugin_sort_fn(a, b):
            """Sorts the plugins.

            All plugins are equals, apart from the Sonar Runner which should be
            executed last.

            Returns a negative, zero or positive number depending on whether
            the first argument is considered smaller than, equal to, or larger
            than the second argument.

            :param a: first plugin
            :type a: GNAThub.Plugin
            :param b: second plugin
            :type b: GNAThub.Plugin
            :return: -1, 0 or 1 depending on the input
            :rtype: int
            """

            if a().name == PluginRunner.SONAR_RUNNER:
                return 1
            if b().name == PluginRunner.SONAR_RUNNER:
                return -1
            return 0

        return sorted(plugins, plugin_sort_fn)

    @staticmethod
    def walk_repository(repository):
        """Walks a script repository, and gathers the scripts is contains

        Lists all Python scripts that are located at the root of the
        repository. If the basename of a script starts with an underscore
        ("_"), it is considered private and thus won't be in the returned list.

        This method is a generator, which yields on every script it finds.

        :param repository: the path to the repository to inspect
        :type repository: str
        """

        for script in os.listdir(repository):
            plugin, ext = os.path.splitext(script)

            if ext == PluginRunner.PLUGIN_EXT and not plugin.startswith('_'):
                yield os.path.join(repository, script)

    @staticmethod
    def inspect(script):
        """Inspects a Python script for :class:`GNAThub.Plugin` it declares

        This method should be used as a generator. It will yield on new every
        new plugin it will find during its inspection of the script.

        :param script: path to the Python script to load
        :type script: str
        """

        if not os.path.isfile(script):
            PluginRunner.warn('%s: not a valid script' % script)
            return

        namespace = {}

        try:
            LOG.debug('  + %s', script)
            execfile(script, namespace)

        except Exception as why:
            LOG.exception('failed to load script: %s', script)
            PluginRunner.warn('%s: failed to load: %s' % (script, str(why)))

        for _, obj in namespace.items():
            if inspect.isclass(obj) and obj.__base__ is GNAThub.Plugin:
                yield obj

    @staticmethod
    def auto_discover_plugins():
        """Retrieves all plugins for GNAThub

        This routine first lists all available scripts for this run of GNAThub.
        It then tries to load each one of them and collect any Plugin declared
        in those scripts.

        This list of plugins is then filtered given the parameters of the run,
        ie.:
            * If the switch --plugins is supplied on the command line, execute
              only plugins whose name is in this list;
            * Otherwise, if the project file contains the GNATdashboard.Plugins
              attribute, execute only plugins whose name is in this list;
            * Otherwise, execute all available plugins.

        :return: the list of plugins available in the current environment
        :rtype: list[GNAThub.Plugin]
        """

        # Locate all Python scripts that might hold the definition of one or
        # more plugins.

        scripts = set()

        for name, path in GNAThub.repositories().items():
            if not os.path.isdir(path):
                LOG.info('skip repository [%s] (not found)', name)
                continue

            LOG.info('load scripts from [%s] repository', name)
            repo_scripts = list(PluginRunner.walk_repository(path))

            LOG.info('  + %d new script(s) found', len(repo_scripts))
            scripts.update(repo_scripts)

            if len(repo_scripts):
                # Add the repository to sys.path so that modules can import
                # themselves. Do this only if scripts were found in this
                # repository.
                sys.path.append(path)

        # Compute the plugins list given the program input and their priority.
        # Priority order:
        #       1. Command line
        #       2. Project file
        #       3. All discoverable plugins

        if GNAThub.plugins():
            LOG.info('use user-defined list of plugins (--plugins switch)')
            explicit = [p.strip() for p in GNAThub.plugins().split(',')]

        elif GNAThub.Project.property_as_list('Plugins'):
            LOG.info('use project-defined list of plugins (attr. Plugins)')
            explicit = GNAThub.Project.property_as_list('Plugins')

        else:
            LOG.info('use all discoverable plugins')
            explicit = None

        # Generate the final list of plugin classes. Inspect Python scripts to
        # extract class definition and filter out those that will not be used.

        LOG.info('load %d scripts', len(scripts))
        plugins = sum([list(PluginRunner.inspect(s)) for s in scripts], [])

        def is_plugin(clazz, name):
            """Checks whether this plugin name is ``name``

            :param clazz: the plugin type object
            :type clazz: type
            :param name: the expected name
            :type name: str
            :return: ``True`` if this plugin name is ``name``
            :rtype: boolean
            """

            return (clazz.__name__.lower() == name.lower() or
                    clazz().name.lower() == name.lower())

        def contains_plugin_name(clazz, names):
            """Checks whether the plugin name is in ``names``

            :param clazz: the plugin type object
            :type clazz: type
            :param names: the list of name
            :type names: list[str]
            :return: ``True`` if the plugin name is in ``names``
            :rtype: boolean
            """

            for name in names:
                if is_plugin(clazz, name):
                    return True

            return False

        if explicit:
            # Cleanup user input if needed. The following statement remove
            # None and empty string values as well as duplicates. It also set
            # the plugin name to lower case for future comparison.
            explicit = set([p.lower() for p in explicit if p])

            # Filter out any plugin whose name is not in the "explicit" set
            plugins = [c for c in plugins if contains_plugin_name(c, explicit)]

        # Remove explicitly disabled plugins
        for name in GNAThub.Project.property_as_list('Plugins_Off'):
            for clazz in plugins:
                if is_plugin(clazz, name):
                    LOG.info('disable %s [Plugin_Off]', name)
                    plugins.remove(clazz)
                    break

            LOG.warn('%s explicitly disabled but not loaded', name)

        return PluginRunner.schedule(plugins)

    @staticmethod
    def execute(plugin):
        """Executes the plugin

        Call methods setup, execute and teardown for a plugin instance.

        :param plugin: instance of the plugin to execute
        :type plugin: GNAThub.Plugin
        """

        start = time.time()

        PluginRunner.info('execute plug-in %s' % plugin.name)

        LOG.info('%s: setup environment', plugin.name)
        plugin.setup()

        plugin.execute()

        LOG.info('%s: post execution', plugin.name)
        plugin.teardown()

        elapsed = time.time() - start

        if plugin.exec_status == GNAThub.EXEC_SUCCESS:
            plugin.info('completed (in %d seconds)' % elapsed)

        elif plugin.exec_status == GNAThub.EXEC_FAILURE:
            plugin.error('execution failed')

        elif plugin.exec_status != GNAThub.NOT_EXECUTED:
            plugin.error('unknown plug-in execution code: %d' %
                         plugin.exec_status)
        else:
            plugin.info('not executed')

    def mainloop(self):
        """Plugins main loop"""

        LOG.info('load (%d) plugins', len(self.plugins))

        if not self.plugins:
            PluginRunner.info('nothing to do')
            return

        for plugin in self.plugins:
            LOG.info('  + %s', plugin.name)

        for plugin in self.plugins:
            try:
                PluginRunner.execute(plugin())

            except KeyboardInterrupt:
                Console.info('%sinterrupt caught...' % os.linesep)

            except Exception as why:
                LOG.exception('plug-in execution failed')
                PluginRunner.error('unexpected error: %s' % why)


# Script entry point
if __name__ == '__main__':
    formatter = logging.Formatter(
        fmt='[%(asctime)s.%(msecs)03d %(name)s] %(message)s',
        datefmt='%H:%M:%S')

    handler = GNAThubLoggingHandler()
    handler.setFormatter(formatter)

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG)
    root_logger.addHandler(handler)

    PluginRunner().mainloop()
