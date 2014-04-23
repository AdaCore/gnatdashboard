##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                     Copyright (C) 2013-2014, AdaCore                     ##
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

"""This module defines the core components of GNAThub plugin mechanism.

It declares module routines and classes implemented in Ada and exported to
Python.

In particular the :class:`GNAThub.Plugin` is the base class to use for writing
plug-ins.

"""


def root():
    """Returns the path to the GNAThub-specific root directory.
    Usually:

        <project_object_dir>/gnathub

    :returns: str

    """

    pass        # Implemented in Ada


def logs():
    """Returns the path to the GNAThub-specific directory for logs.
    Usually:

        <project_object_dir>/gnathub/logs

    :returns: str

    """

    pass        # Implemented in Ada


def jobs():
    """Returns the number of parallel jobs to execute (equivalent to -j).

    :returns: int

    """

    pass        # Implemented in Ada


def plugins():
    """Returns the list of comma-separated plug-ins name specified on the
    command-line.

    :returns: str

    """

    pass        # Implemented in Ada


def repositories():
    """Returns the list of available repositories.

    The dictionary contains 3 keys:
        * system
        * global
        * local

    These repositories correspond respectively to the [core] and [extra]
    directories from the GNAThub installation, and the Local_Repository the
    user can specify in its project file.

    :returns: dict[str, str]

    """

    pass        # Implemented in Ada


def database():
    """Returns the path to the GNAThub SQLite database.
    Usually:

        <project_object_dir>/gnathub/gnathub.db

    :returns: str

    """

    pass        # Implemented in Ada


class Logger(object):
    """A logger object. Fully implemented in Ada."""

    def __init__(self, name):
        """Instance constructor."""

        pass    # Implemented in Ada

    def info(self, message):
        """Prints an informative message."""

        pass    # Implemented in Ada

    def warn(self, message):
        """Prints a warning message."""

        pass    # Implemented in Ada

    def error(self, message):
        """Prints an error message."""

        pass    # Implemented in Ada

    def fatal(self, message):
        """Prints a fatal message."""

        pass    # Implemented in Ada

    def debug(self, message):
        """Prints a debug message."""

        pass    # Implemented in Ada


class Console(object):
    """Provides several helper routines implemented in Ada."""

    @staticmethod
    def info(message, prefix=None):
        """Prints an informative message. Activated at default verbosity."""

        pass    # Implemented in Ada

    @staticmethod
    def warn(message, prefix=None):
        """Prints a warning message. Activated at default verbosity output."""

        pass    # Implemented in Ada

    @staticmethod
    def error(message, prefix=None):
        """Prints an error message. Always activated."""

        pass    # Implemented in Ada

    @staticmethod
    def progress(current, natural, new_line=False):
        """Prints a progress message. Activated at default verbosity level.  If
        new_line is True, then terminate the line with a '\n' character.

        Defaults to False.

        """

        pass    # Implemented in Ada


class Project(object):
    """A project namespace. Fully implemented in Ada."""

    def __init__(self):
        """Instance constructor."""

        raise Error('GNAThub.Project must not be instantiated')

    @staticmethod
    def name():
        """Returns the name of the root project.

        :returns: str

        """

        pass    # Implemented in Ada

    @staticmethod
    def path():
        """Returns the full path to the root project.

        :returns: str

        """

        pass    # Implemented in Ada

    @staticmethod
    def object_dir():
        """Returns the full path to the root project object directory.

        :returns: str

        """

        pass    # Implemented in Ada

    @staticmethod
    def source_dirs():
        """Returns the list of source directories for each project.

        :returns: dict[str, list[str]]

        """

        pass    # Implemented in Ada

    @staticmethod
    def source_file(name):
        """Create a new file. This will automatically try to solve Name to an
        absolute path if it currently is a base name.  If Name is an absolute
        path, it is returned as is. Otherwise, only the base name is used (ie.
        we remove any directory information from Name).

        :returns: str

        """

        pass    # Implemented in Ada

    @staticmethod
    def property_as_string(key):
        """Returns the string representation of the project property from the
        package GNATdashboard.

        :returns: str

        """

        pass    # Implemented in Ada

    @staticmethod
    def property_as_list(key):
        """Returns the list of string representation of the project property
        from the package GNATdashboard.

        :returns: list[str]

        """

        pass    # Implemented in Ada


# Install all Ada extensions, i.e. functions and classes implemented in Ada and
# exported to Python. These extensions should be declared above this statement
# with no implementation.

import logging

try:
    # pylint: disable=wildcard-import, import-error
    from GNAThubCore import *       # NOQA (disable warning from flake8)

except ImportError:
    logging.warn('Failed to import Ada implementation of the module.')
    logging.warn('This module was likely not loaded by the GNAThub driver.')
    logging.warn('This may be expected. GNAThub module implementation will'
                 'not be available (stubs will not be populated).')

# Now that all Ada extensions have been planted into this module, we can
# define pure-Python extensions.

import os

from abc import ABCMeta, abstractmethod, abstractproperty
from subprocess import Popen, STDOUT

EXEC_SUCCESS, EXEC_FAILURE, NOT_EXECUTED = range(3)

# Database-related constants
RULE_KIND, METRIC_KIND = range(2)
PROJECT_KIND, DIRECTORY_KIND, FILE_KIND = range(3)


class Error(Exception):
    """Base class for exceptions in this module."""
    pass


# pylint: disable=old-style-class,abstract-class-not-used
class Plugin:
    """GNAThub plugin interface.

    A plugin is a Python class that describe how to configure, run and collect
    data output by an external tool.
    Each plugin should be dedicated to only one tool.

    To implement a now plugin, simply creates a new Python class inheriting
    from this (GNAThub.Plugin) abstract base class.

    All plugins are collected using the inheritance mechanism, i.e. the GNAThub
    driver will automatically find all classes implementing the GNAThub.Plugin
    interface. No manual registration needed.

    """

    __metaclass__ = ABCMeta

    def __init__(self):
        """Instance constructor."""

        # A custom instance of a logger. It is implemented in Ada and based on
        # the GNATCOLL.Traces module.
        self.log = logging.getLogger(self.name or self.__class__.__name__)

        # The execution status of this plugin. Initialized to NOT_EXECUTED.
        # Should be set to EXEC_FAILURE or EXEC_SUCCESS by the plugin in the
        # Plugin.execute method.
        self._exec_status = NOT_EXECUTED

    @abstractproperty
    def name(self):
        """Returns the name of the tool, as specified by the TOOL_NAME class
        variable.

        :returns: str

        """

        pass

    def info(self, message):
        """Displays an informative message, prefixed with the plug-in name."""

        Console.info(message, prefix=self.name)

    def warn(self, message):
        """Displays a warning message, prefixed with the plug-in name."""

        Console.warn(message, prefix=self.name)

    def error(self, message):
        """Displays an error message, prefixed with the plug-in name."""

        Console.error(message, prefix=self.name)

    def setup(self):
        """This method is called prior to a call to Plugin.execute.

        This is where environment setup should be done to ensure a correct
        execution of the tool.

        """

        pass

    @abstractmethod
    def execute(self):
        """Abstract method. Need implementation.

        Executes the external tool. This method is called after setup() and
        before teardown().

        """

        pass

    def teardown(self):
        """This method is called after a call to Plugin.execute.

        This is where environment cleanup should be done to ensure a consistent
        state for a future execution.

        """

        pass

    @property
    def exec_status(self):
        """Returns the execution status for the tool.

        Can be one of the following:
            GNAThub.NOT_EXECUTED: plugin did not run yet
            GNAThub.EXEC_FAILURE: an error occurred during the plugin execution
            GNAThub.EXEC_SUCCESS: the plugin execution completed successfully

        :returns: int

        """

        return self._exec_status

    @exec_status.setter
    def exec_status(self, status):
        """Sets the execution status for the tool.

        Can be one of the following:
            GNAThub.NOT_EXECUTED: plugin did not run yet
            GNAThub.EXEC_FAILURE: an error occurred during the plugin execution
            GNAThub.EXEC_SUCCESS: the plugin execution completed successfully

        :param int status: The new execution status.

        """

        if status not in (EXEC_FAILURE, EXEC_SUCCESS, NOT_EXECUTED):
            raise Error('invalid execution status code')

        self._exec_status = status


class Run(object):
    """Class to handle processes."""

    # pylint: disable=too-many-arguments
    def __init__(self, name, argv, env=None, workdir=None, out=None):
        """Instance constructor.

        Spawns the process via subprocess.Popen and returns the process exit
        code.

        :param str name: the name of the executable.
        :param list[str] argv: the argument array.
        :param dict[str, str] env: Dictionary containing the environment to
            pass through to the process. If None, os.environ is used.
        :param str workdir: The directory in which to execute the process. If
            None, use the current directory.
        :param str out: The log file to use.
        :returns: int

        """

        self.name = name
        self.argv = argv
        self.status = 127
        self.pid = -1
        self.out = out
        self.log = logging.getLogger(self.__class__.__name__)

        self.log.debug('Run: cd %s; %s',
                       workdir if workdir is not None else os.getcwd(),
                       self.cmdline_image())

        try:
            with open(self.output(), 'w') as output:
                self.internal = Popen(argv, env=env, stdin=None, stdout=output,
                                      stderr=STDOUT, cwd=workdir)

                Console.info('output redirected to %s' % output.name)
                self.pid = self.internal.pid
                self.wait()

        except OSError as why:
            import errno
            executable = self.argv[0]

            if why.errno == errno.ENOENT:
                Console.error('%s not installed or not in PATH' % executable)
            else:
                Console.error('%s: %s' % (executable, str(why)))

        except Exception as why:
            Console.error(str(why))
            raise

    def wait(self):
        """Waits until process ends and return its status."""

        self.status = self.internal.wait()
        return self.status

    @staticmethod
    def quote(arg):
        """Returns the quoted version of the given argument.

        :param str arg: The argument to quote.
        :returns: str

        """

        specials = ('|', '&', ';', '<', '>', '(', ')', '$', '`', '\\', '"',
                    "'", ' ', '\t', '\n', '*', '?', '[', '#', '~')

        for char in specials:
            if char in arg:
                arg = arg.replace("'", r"'\''")
                arg = arg.replace('\n', r"'\n'")
                return "'%s'" % arg

        return arg

    def cmdline_image(self):
        """Returns a string image of the given command.

        :returns: str

        """

        return ' '.join((Run.quote(arg) for arg in self.argv))

    def output(self):
        """Returns the path to the output file.

        :returns: str

        """

        return self.out or os.path.join(logs(), self.name + '.log')
