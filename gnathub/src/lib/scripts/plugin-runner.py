"""Execute GNAThub-specific plug-ins

Scans for GNAThub-specific plug-ins and registers their execution before
spawning the main event loop.
"""

# GNAThub (GNATdashboard)
# Copyright (C) 2013-2020, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

import inspect
import json
import logging
import os
import sys
import time

import GNAThub
from GNAThub import Console


# Create this script logger
__file__ = inspect.getfile(inspect.currentframe())
MODULE, _ = os.path.splitext(os.path.basename(__file__))
LOG = logging.getLogger(MODULE)


class GNAThubLoggingHandler(logging.Handler):

    """Custom logging handler that uses :class:`GNAThub.Logger` as back-end."""

    def __init__(self):
        """Initialize handler properties."""
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
            logger.log(message)
        except KeyboardInterrupt:
            raise
        except Exception:
            self.handleError(record)
            return


class PluginRunner(object):
    """Class that loads python plugins.

    Retrieve all plugins: core and user

    Plugin execution order:
      * core plugin
      * user plugin
      * sonar plugin (if active)
    """

    PLUGIN_EXT = '.py'
    POST_PHASE_PLUGINS = ('sonar-scanner', 'html-report')

    def __init__(self):
        """Gather the list of plugins."""
        # The list of plugins to be sequentially executed
        self.plugins = PluginRunner.auto_discover_plugins()

    @staticmethod
    def info(message, *args):
        """Display an informative message, prefixed with the plug-in name.

        :param str message: the message to log
        :param list[*] args: arguments of the `message` format string
        """
        Console.info(message % args, prefix=MODULE)

    @staticmethod
    def warn(message, *args):
        """Display a warning message, prefixed with the plug-in name.

        :param str message: the message to log
        :param list[*] args: arguments of the `message` format string
        """
        Console.warn(message % args, prefix=MODULE)

    @staticmethod
    def error(message, *args):
        """Display an error message, prefixed with the plug-in name.

        :param str message: the message to log
        :param list[*] args: arguments of the `message` format string
        """
        Console.error(message % args, prefix=MODULE)

    @classmethod
    def schedule(cls, plugins):
        """Schedule the plugins execution order.

        Some system plugins might need to be executed in a specific order, or
        for example, for the Sonar Runner plugin, last. This routine takes care
        of ordering the plugins in their final execution order.

        :param collections.Iterable[GNAThub.Plugin] plugins: list of plugins to
            be executed
        :return: the ordered list of plugins
        :rtype: collection.Iterable[GNAThub.Plugin]
        """
        if GNAThub.dry_run_without_project():
            #  The list of predefined plugins needs to be gathered, any
            #  sorting is necessary at that point
            return plugins
        else:
            return sorted(plugins,
                          key=lambda p: p.name in cls.POST_PHASE_PLUGINS)

    @classmethod
    def walk_repository(cls, repository):
        """Walk a script repository, and gathers the scripts is contains.

        Lists all Python scripts that are located at the root of the
        repository. If the basename of a script starts with an underscore
        ("_"), it is considered private and thus won't be in the returned list.

        This method is a generator, which yields on every script it finds.

        :param str repository: the path to the repository to inspect
        """
        for script in os.listdir(repository):
            plugin, ext = os.path.splitext(script)

            if ext == cls.PLUGIN_EXT and not plugin.startswith('_'):
                yield os.path.join(repository, script)

    @classmethod
    def inspect(cls, script):
        """Inspect a Python script for :class:`GNAThub.Plugin` it declares.

        This method should be used as a generator. It will yield on new every
        new plugin it will find during its inspection of the script.

        :param str script: path to the Python script to load
        """
        if not os.path.isfile(script):
            cls.warn('%s: not a valid script', script)
            return

        namespace = {}

        try:
            LOG.debug('  + %s', script)
            exec(compile(open(script).read(), script, 'exec'), namespace)

        except Exception as why:
            LOG.exception('failed to load script: %s', script)
            cls.warn('%s: failed to load: %s', script, str(why))

        for obj in list(namespace.values()):
            if inspect.isclass(obj) and obj.__base__ is GNAThub.Plugin:
                yield obj

    @classmethod
    def auto_discover_plugins(cls):
        """Retrieve all plugins for GNAThub.

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
        :rtype: collections.Iterable[GNAThub.Plugin]
        """
        # Locate all Python scripts that might hold the definition of one or
        # more plugins.
        scripts = set()

        for name, path in list(GNAThub.repositories().items()):
            if not os.path.isdir(path):
                LOG.info('skip repository [%s] (not found)', name)
                continue

            LOG.info('load scripts from [%s] repository', name)
            repo_scripts = list(cls.walk_repository(path))

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

        # If no object directory was created and --dry-run means that gnathub
        # was called in discovery mode of predefined plugins
        if GNAThub.dry_run_without_project():
            explicit = None

        else:
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

        LOG.info('located %d scripts', len(scripts))
        plugins = sum([list(cls.inspect(s)) for s in scripts], [])

        def is_plugin(clazz, name):
            """Check whether this plugin name is ``name``.

            :param type clazz: the plugin type object
            :param str name: the expected name
            :return: ``True`` if this plugin name is ``name``
            :rtype: boolean
            """
            return (
                clazz.__name__.lower() == name.lower() or
                clazz().name.lower() == name.lower()
            )

        def contains_plugin_name(clazz, names):
            """Check whether the plugin name is in ``names``.

            :param type clazz: the plugin type object
            :param collections.Iterable[str] names: the list of name
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

        # Return all autodiscovered plugins if is --dry_run mode without
        # project file as command line parameter
        if GNAThub.dry_run_without_project():
            return cls.schedule(plugins)

        # Remove explicitly disabled plugins
        for name in GNAThub.Project.property_as_list('Plugins_Off'):
            for clazz in plugins:
                if is_plugin(clazz, name):
                    LOG.info('disable %s [Plugin_Off]', name)
                    plugins.remove(clazz)
                    break

            LOG.warn('%s explicitly disabled but not loaded', name)

        return cls.schedule(plugins)

    @staticmethod
    def execute_runners():
        """Whether to execute plugins implementing :class:`GNAThub.Runner`.

        --runners-only and --reporters-only are mutually exclusive. Runners
        should be executed if --runners-only is specified or if none is
        specified.

        :rtype: boolean
        """
        return GNAThub.runners_only() or not GNAThub.reporters_only()

    @staticmethod
    def execute_reporters():
        """Whether to execute plugins implementing :class:`GNAThub.Reporter`.

        --runners-only and --reporters-only are mutually exclusive. Runners
        should be executed if --reporters-only is specified or if none is
        specified.

        :rtype: boolean
        """
        return GNAThub.reporters_only() or not GNAThub.runners_only()

    @staticmethod
    def is_runner(plugin):
        """Whether plugin implements :class:`GNAThub.Runner`.

        :rtype: boolean
        """
        return isinstance(plugin, GNAThub.Runner)

    @staticmethod
    def is_reporter(plugin):
        """Whether plugin implements :class:`GNAThub.Reporter`.

        :rtype: boolean
        """
        return isinstance(plugin, GNAThub.Reporter)

    @classmethod
    def should_execute(cls, plugin):
        """Whether a plugin should be executed or not.

        :rtype: boolean
        """
        return ((cls.execute_runners() and cls.is_runner(plugin)) or
                (cls.execute_reporters() and cls.is_reporter(plugin)))

    @classmethod
    def execute(cls, plugin):
        """Execute the plugin.

        Call methods setup, execute and teardown for a plugin instance.

        :param GNAThub.Plugin plugin: instance of the plugin to execute
        :return: the execution time in seconds
        :rtype: int
        """
        elapsed = 0

        if cls.should_execute(plugin):
            cls.info('execute plug-in %s', plugin.name)
            if GNAThub.dry_run():
                # Early exit if dry-run mode is enabled
                plugin.exec_status = GNAThub.EXEC_SUCCESS
                return 0

            LOG.info('%s: set up environment', plugin.name)
            start = time.time()
            plugin.setup()

            if cls.execute_runners() and cls.is_runner(plugin):
                LOG.info('%s: produce results', plugin.name)
                plugin.exec_status = plugin.run()

            if (cls.execute_reporters() and cls.is_reporter(plugin) and
                    plugin.exec_status in (
                        GNAThub.EXEC_SUCCESS, GNAThub.NOT_EXECUTED)):
                LOG.info('%s: collect results', plugin.name)
                plugin.exec_status = plugin.report()

            LOG.info('%s: post execution', plugin.name)
            plugin.teardown()
            elapsed = time.time() - start

        if plugin.exec_status == GNAThub.EXEC_SUCCESS:
            plugin.info('completed (in %d seconds)' % elapsed)
        elif plugin.exec_status == GNAThub.EXEC_FAILURE:
            plugin.error('execution failed')
        else:
            assert plugin.exec_status == GNAThub.NOT_EXECUTED
            plugin.info('not executed')
        return elapsed

    def mainloop(self):
        """Plugin main loop."""
        LOG.info('registered %d plugins', len(self.plugins))
        backlog = []

        # Early exit if no plug-in are scheduled to be run
        if not self.plugins:
            self.info('nothing to do')
            return

        # Early exit when --dry_run mode without project file
        # and dump the list of plugins
        if GNAThub.dry_run_without_project():
            for cls in self.plugins:
                plugin = cls()
                self.info('%s plug-in is available', plugin.name)
                plugin.exec_status = GNAThub.EXEC_SUCCESS
            return

        # Execute each plug-in in order
        exec_failure = False
        try:
            for cls in self.plugins:
                try:
                    # Create a new instance
                    plugin, elapsed = cls(), None
                    # Execute the plug-in
                    elapsed = self.execute(plugin)
                except KeyboardInterrupt:
                    raise
                except Exception as why:
                    LOG.exception('plug-in execution failed')
                    self.error('%s: unexpected error: %s', plugin.name, why)
                finally:
                    if plugin.exec_status != GNAThub.NOT_EXECUTED:
                        # A plugin could not have been executed depending on
                        # the command line (--runners-only/--reporters-only).
                        backlog.append((plugin.name, {
                            'time': elapsed or 0,
                            'success': (
                                plugin.exec_status == GNAThub.EXEC_SUCCESS)
                        }))

                        # Compute all plugins execution status
                        exec_failure = (exec_failure or
                                        (plugin.exec_status ==
                                         GNAThub.EXEC_FAILURE))

        except KeyboardInterrupt:
            self.info(os.linesep + 'Interrupt caught...')

        # Write results to file
        fname = os.path.join(GNAThub.root(), 'gnathub.backlog')
        try:
            with open(fname, 'w') as fd:
                fd.write(json.dumps(backlog))
        except IOError as why:
            LOG.exception('could not write result file %s', fname)
            self.error('%s: unexpected error: %s', fname, why)

        if not GNAThub.dry_run() and not GNAThub.quiet():
            # Display a summary
            for plugin, results in backlog:
                if results['success']:
                    Console.ok(plugin)
                else:
                    Console.ko(plugin)

        if exec_failure:
            self.error('GNAThub error: one or more plugins failed to run!')


# Script entry point
if __name__ == '__main__':
    handler = GNAThubLoggingHandler()
    handler.setFormatter(logging.Formatter(fmt='%(message)s'))

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG)
    root_logger.addHandler(handler)

    PluginRunner().mainloop()
