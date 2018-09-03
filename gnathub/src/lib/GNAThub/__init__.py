"""This module defines the core components of GNAThub plugin mechanism.

It declares module routines and classes implemented in Ada and exported to
Python.

In particular the :class:`GNAThub.Plugin` is the base class to use for writing
plug-ins.
"""

# GNAThub (GNATdashboard)
# Copyright (C) 2013-2017, AdaCore
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


def root():
    """Return the path to the GNAThub-specific root directory.

    Usually :file:`<project_object_dir>/gnathub`.

    :return: the full path to the directory
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def logs():
    """Return the path to the GNAThub-specific directory for logs.

    Usually :file:`<project_object_dir>/gnathub/logs`.

    :return: the full path to the log directory
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def html_data():
    """Return the path to the GNAThub-specific directory for HTML report data.

    Usually :file:`<project_object_dir>/gnathub/html-report/data`.

    :return: the full path to the log directory
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def dry_run():
    """Whether to run in "check mode" or not.

    :return: whether the dry-run flag is enabled or not
    :rtype: bool
    """
    return NotImplemented   # Implemented in Ada


def quiet():
    """Whether the quiet flag was passed to the GNAThub driver or not.

    :return: whether the quiet flag is enabled or not
    :rtype: bool
    """
    return NotImplemented   # Implemented in Ada


def verbose():
    """Whether the verbose flag was passed to the GNAThub driver or not.

    :return: whether the verbosity flag is enabled or not
    :rtype: bool
    """
    return NotImplemented   # Implemented in Ada


def jobs():
    """Return the number of parallel jobs to execute.

    This is the equivalent to using :command:`-j` on the command-line.

    :return: the maximum number of separate processes to spawn
    :rtype: int
    """
    return NotImplemented   # Implemented in Ada


def subdirs():
    """Return the name of the new object directory provided with the switch.

    This is the equivalent to using :command:`--subdirs` on the command-line.

    :return: the name of the new object directory
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada

# Keeping this for later implementation of -U main switch
# def u_main():
#     """Return the name of the main file provided with the switch.
#
#     This is the equivalent to using :command:`-U <main_file>` on command-line
#
#     :return: the name of the new object directory
#     :rtype: str
#     """
#     return NotImplemented   # Implemented in Ada


def u_process_all():
    """Whether the -U switch was passed to the GNAThub driver or not.

    :return: whether -U switch is enabled or not
    :rtype: bool
    """
    return NotImplemented   # Implemented in Ada


def plugins():
    """Return the list of comma-separated plug-in names.

    This is the list of plug-ins as specified on the command-line with the
    :command:`--plugins` switch.

    :return: the list of plug-in name
    :rtype: collections.Iterable[str]
    """
    return NotImplemented   # Implemented in Ada


def port():
    """Return the port number provided with the switch.

    This is the equivalent to using :`--port` switch on the command-line.

    :return: the port number
    :rtype: int
    """
    return NotImplemented   # Implemented in Ada


def repositories():
    """Return the list of available repositories.

    The dictionary contains 3 keys:

    * :command:`system`
    * :command:`global`
    * :command:`local`

    These repositories correspond respectively to the ``[core]`` and
    ``[extra]`` directories from the GNAThub installation, and the
    :command:`Local_Repository` the user can specify in its project file.

    :return: the available repositories details
    :rtype: dict[str, str]
    """
    return NotImplemented   # Implemented in Ada


def engine_repository():
    """Return the ``engine`` repository

    :return: the repository path
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def database():
    """Return the path to the GNAThub SQLite database.

    Usually :file:`<project_object_dir>/gnathub/gnathub.db`.

    :return: the full path to the local serialized SQLite database
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def output_dir():
    """Return the Codepeer output repository

    :return: the repository path
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def db_dir():
    """Return the Codepeer DB repository

    :return: the repository path
    :rtype: str
    """
    return NotImplemented   # Implemented in Ada


def tool_args(tool_name):
    """Return the list of extra switches to pass to an inferior tool.

    This is the concatenation of switches for the tool ``tool_name`` as
    provided on the command-line with the :command:`-targs:` switch.

    :param str tool_name: the name of the tool
    :return: the list of extra switches for ``tool_name``
    :rtype: collections.Iterable[str]
    """
    return NotImplemented   # Implemented in Ada


class Logger(object):

    """A logger object. Fully implemented in Ada."""

    def __init__(self, name):
        """Create a new Ada logger object.

        From :program:`GNAThub` plug-ins, prefer the use of :mod:`logging`
        instead of this logger class. A custom :class:`logging.Handler` is
        automatically installed to use :mod:`logging` as logging front-end
        and :class:`GNAThub.Logger` as logging back-end.

        :param str name: the name of the logger
        """
        pass    # Implemented in Ada

    def log(self, message):
        """Log a message.

        :param str message: the message to log
        """
        pass    # Implemented in Ada


class Console(object):

    """Provide several helper routines implemented in Ada."""

    @staticmethod
    def info(message, prefix=None):
        """Print an informative message.

        Activated at default verbosity.

        :param str message: the message to display
        :param str prefix: optional prefix to the message
        """
        pass    # Implemented in Ada

    @staticmethod
    def warn(message, prefix=None):
        """Print a warning message.

        Activated at default verbosity output.

        :param str message: the message to display
        :param str prefix: optional prefix to the message
        """
        pass    # Implemented in Ada

    @staticmethod
    def error(message, prefix=None):
        """Print an error message.

        Always activated.

        :param str message: the message to display
        :param str prefix: optional prefix to the message
        """
        pass    # Implemented in Ada

    @staticmethod
    def progress(current, total, new_line=False):
        r"""Print a progress message.

        Activated at default verbosity level. If ``new_line`` is
        :command:`True`, then terminates the line with a :kbd:`\n` character.

        :param int current: the current value
        :param int total: the total value
        :param bool new_line: whether to terminate with a new line
        """
        pass    # Implemented in Ada


class Project(object):

    """A project namespace, fully implemented in Ada."""

    def __init__(self):
        """Instance constructor.

        Do not use directly.
        """
        raise Error('GNAThub.Project must not be instantiated manually')

    @staticmethod
    def name():
        """Return the name of the root project.

        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def target():
        """Return the target of the root project.

        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def runtime():
        """Return the runtime of the root project.

        This concerns only the runtime for Ada.

        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def path():
        """Return the full path to the root project.

        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def object_dir():
        """Return the full path to the root project object directory.

        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def object_dirs():
        """Return the list of object directories in the project tree.

        :return: the list of object directories for the current project
        :rtype: dict[str, list[str]]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def source_dirs():
        """Return the list of source directories for each project.

        :return: the list of all sources directories per project
        :rtype: dict[str, list[str]]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def source_files():
        """Return the list of source files for each project.

        :return: the list of all sources files per project
        :rtype: list[str]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def source_file(name):
        """Create a new file.

        This will automatically try to solve ``name`` to an absolute path if it
        currently is a base name. If ``name`` is an absolute path, it is
        returned as is. Otherwise, only the base name is used (i.e. we remove
        any directory information from ``name``).

        :param str name: the source file basename
        :return: the full path to the source file
        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def source_suffixes(language):
        """Return a list of Ada source file suffixes.

        The list is used by the project manager to find Ada source files, ie.
        both specifications and implementations.

        :param str language: the language of the sources
        :return: the list of valid Ada source file extensions
        :rtype: collections.Iterable[str]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def property_as_string(key, package=None):
        """Return a project property.

        Returns the string representation of the project property from the
        package GNATdashboard.

        :param str key: the property name
        :param str package: the package name (default to GNATdashboard package)
        :return: the property value
        :rtype: str
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def property_as_list(key, package=None):
        """Return a project property.

        Returns the list of string representation of the project property from
        the package GNATdashboard.

        :param str key: the property name
        :param str package: the package name (default to GNATdashboard package)
        :return: the property value
        :rtype: list[str]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def scenario_switches():
        """Return the scenario as a list of switches of the form -Xvar=value.

        :return: the list of scenario switches passed to GNAThub
        :rtype: collections.Iterable[str]
        """
        return NotImplemented   # Implemented in Ada


class Tool(object):

    """A Tool object, mapping to a Tool entry in the database."""

    __slots__ = ('id', 'name')

    def __init__(self, name):
        """Return the tool of the given name, creating it if necessary.

        :param str name: the name of the tool to create or retrieve
        """
        pass    # Implemented in Ada

    @staticmethod
    def list():
        """List all the tools stored in the database.

        :return: the list of all tools
        :rtype: collections.Iterable[GNAThub.Tool]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def clear_references(tool):
        """Clear all references to a tool in the database.

        :param str tool: the name of the tool
        """
        return NotImplemented   # Implemented in Ada

    def add_messages(self, resources_messages, entities_messages):
        """Add resources and entities (if any) messages to the given tool.

        Prefer this function when there are many messages to insert, for
        efficiency.
        ``resources_messages`` is a list, each entry being a list
        of the form::

            [resource, message_data]

        where ``message_data`` is a list and each entry is of the form

            [message, line, col_begin, col_end]

        ``message`` is the :class:`GNAThub.Message` to add.
        ``line``, ``col_begin``, ``col_end``: int.

        Example::

            tool.add_messages(
                    [[resource1, [[ message, 1, 2, 2 ],
                                 [ message, 2, 1, 1 ],
                                 [ message, 0, 1, 1 ]]],
                     [resource2, [[ message, 10002, 1, 1 ],
                                 [ message, 10003, 1, 1 ],
                                 [ message, 10005, 1, 1 ]]]],
                    [[entity1, [[message, 1, 2, 2]]],
                     [entity2, [[ message, 1, 2, 2 ],
                                [ message, 2, 1, 1 ]]]])

        :param collections.Iterable messages: the messages to add
        """
        pass    # implemented in Ada


class Rule(object):

    """A Rule object, representing a rule in the database."""

    __slots__ = ('id', 'name', 'identifier', 'kind', 'tool_id')

    def __init__(self, name, identifier, kind, tool):
        """Return the rule of the given properties, creating it if necessary.

        :param str name: the name of the rule
        :param str identifier: an unique identifier for this rule (typically,
            the same as name)
        :param int kind: RULE_KIND to indicate a rule where messages are given
            without a numeric value, or METRIC_KIND to indicate a rule where
            messages correspond to a numeric value
        :param GNAThub.Tool tool: the tool that defines this rule
        """
        pass    # Implemented in Ada

    @staticmethod
    def list():
        """Return all the rules stored in the database.

        :return: the list of all :class:`GNAThub.Rule`
        :rtype: collections.Iterable[GNAThub.Rule]
        """
        return NotImplemented   # Implemented in Ada


class Property(object):

    """A Property object, corresponding to a message property."""

    __slots__ = ('id', 'identifier', 'name')

    def __init__(self, identifier, name):
        """Return a Property, creating it if necessary.

        :param str identifier: the unique identifier of the property
        :param str name: the display name of the property
        """
        pass    # Implemented in Ada

    @staticmethod
    def list():
        """Return all properties stored in the database.

        :return: the list of all :class:`GNAThub.Property`
        :rtype: collections.Iterable[GNAThub.Property]
        """
        return NotImplemented   # Implemented in Ada


class Message(object):

    """A Message object, representing one message in the database."""

    __slots__ = ('id', 'rule_id', 'data', 'ranking', 'tool_msg_id',
                 'line', 'col_begin', 'col_end')

    def __init__(self, rule, message, ranking=1, tool_msg_id=0,
                 properties=None):
        """Return the message matching the given properties.

        :param GNAThub.Rule rule: the rule to which this message belongs
        :param str message: the data to associate to the message: this should
            be a numeric value if the rule is of METRIC_KIND
        :type ranking: Integer values in 0 to 5 range corresponding to
                       0-'Annotation',
                       1-'Unspecified',
                       2-'Info',
                       3-'Low',
                       4-'Medium',
                       5-'High'
        :param int tool_msg_id: the tool original message id
        :param properties: one or more properties
        :type properties: collections.Iterable[GNAThub.Property] or None
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def list():
        """Return all messages stored in the database.

        :return: the list of all :class:`GNAThub.Message`
        :rtype: collections.Iterable[GNAThub.Message]
        """
        return NotImplemented   # Implemented in Ada


class Resource(object):

    """A Resource object, corresponding to a resource in the database.

    A resource represents either a file, a directory, or a project.
    """

    __slots__ = ('id', 'name', 'kind')

    def __init__(self, name, kind):
        """Return a Resource, creating it if necessary.

        :param str name: the name of the resource. For files and directories,
            this should be a normalized full path: the full path with all
            links resolved, and with the original filesystem casing.
            For a project, this is the cased name of the project
        :param int kind: ``PROJECT_KIND``, ``DIRECTORY_KIND``, ``FILE_KIND``
            for projects, directories, and files, respectively
        """
        pass    # Implemented in Ada

    def add_message(self, message, line=0, col_begin=1, col_end=None):
        """Add a message to the given resource.

        :param GNAThub.Message message: the Message to add
        :param int line: the line to associate the message to, if the resource
            is a file. Use ``0`` to indicate a message which should be
            associated to the resource but not to a specific line
        :param int col_begin: the begin column of the message
        :param int col_end: the end column of the message. ``None`` means that
            the end column should be the same as the begin column.
        """
        pass    # Implemented in Ada

    def add_messages(self, messages):
        """Add multiple messages to the given resource.

        Prefer this function when there are many messages to insert, for
        efficiency. ``messages`` is a list, each entry being a list of the
        form::

            [message, line, col_begin, col_end]

        ``message`` is the :class:`GNAThub.Message` to add.
        ``line``, ``col_begin``, ``col_end``: int, see :meth:`add_message`.

        Example::

            resource.add_messages([
                [ message, 1, 2, 2 ],
                [ message, 2, 1, 1 ],
                [ message, 0, 1, 1 ],
                [ message, 10002, 1, 1 ],
                [ message, 10003, 1, 1 ],
                [ message, 10005, 1, 1 ],
            ])

        :param collections.Iterable messages: the messages to add
        """
        pass    # implemented in Ada

    def list_messages(self):
        """List all messages associated with this resource.

        :return: a list of :class:`GNAThub.Message`
        :rtype: collections.Iterable[GNAThub.Message]
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def get(name):
        """Return the :class:`GNAThub.Resource` with the given name.

        Do not create it if it doesn't exist.

        :param str name: the name of the resource to get
        :return: the :class:`GNAThub.Resource` of that name
        :rtype: GNAThub.Resource
        """
        return NotImplemented   # Implemented in Ada

    @staticmethod
    def list():
        """List all resources stored in the database.

        :return: the list of all :class:`GNAThub.Resource`
        :rtype: collections.Iterable[GNAThub.Resource]
        """
        return NotImplemented   # Implemented in Ada

    def list_entities_messages(self):
        """List all entities messages associated with this resource.

        :return: a list of :class:`GNAThub.Message`
        :rtype: collections.Iterable[GNAThub.Message]
        """
        return NotImplemented   # Implemented in Ada


class Entity(object):

    """An Entity object, representing an entity in the database."""

    __slots__ = ('id', 'name', 'line', 'col_begin', 'col_end', 'resource_id')

    def __init__(self, name, line, col_begin, col_end, resource):
        """Return the entity of the given properties, creating it if necessary.

        :param str name: the name of the entity
        :param GNAThub.Resource resource: the resource that contains entity
        """
        pass    # Implemented in Ada

    @staticmethod
    def list():
        """Return all the entities stored in the database.

        :return: the list of all :class:`GNAThub.Entity`
        :rtype: collections.Iterable[GNAThub.Entity]
        """
        return NotImplemented   # Implemented in Ada

    def add_messages(self, messages):
        """Add multiple messages to the given entity.

        Prefer this function when there are many messages to insert, for
        efficiency.

        :param collections.Iterable messages: the messages to add
        """
        return NotImplemented   # Implemented in Ada

    def list_messages(self):
        """List all messages associated with this entity.

        :return: a list of :class:`GNAThub.Message`
        :rtype: collections.Iterable[GNAThub.Message]
        """
        return NotImplemented   # Implemented in Ada


# Install all Ada extensions, i.e. functions and classes implemented in Ada and
# exported to Python. These extensions should be declared above this statement
# with no implementation.

import logging

try:
    from GNAThubCore import *

except ImportError:
    logging.warn('Failed to import Ada implementation of the module.\n'
                 'This module was likely not loaded by the GNAThub driver.\n'
                 'This may be expected. GNAThub module implementation will'
                 'not be available (stubs will not be populated).')

# Now that all Ada extensions have been planted into this module, we can
# define pure-Python extensions.

import os

from abc import ABCMeta, abstractmethod
from subprocess import Popen, STDOUT

EXEC_SUCCESS, EXEC_FAILURE, NOT_EXECUTED = range(3)

# Database-related constants
RULE_KIND, METRIC_KIND = range(2)
PROJECT_KIND, DIRECTORY_KIND, FILE_KIND = range(3)

# Predefined ranking constants
RANKING_ANNOTATION = 0
RANKING_UNSPECIFIED = 1
RANKING_INFO = 2
RANKING_LOW = 3
RANKING_MEDIUM = 4
RANKING_HIGH = 5


class Error(Exception):

    """Base class for exceptions in this module."""

    pass


def _extend(cls, funcname):
    def decorator(func):
        setattr(cls, funcname, staticmethod(func))
    return decorator


@_extend(Console, '_status')
def _console_status(message, status, columns):
    """Display a status message.

    Execution step ............................... [status]

    :param str message: the message to display
    :param str status: the status to display
    :param int columns: optional maximum column to use for display
    """
    width = columns - len(status) - 4
    if len(message) < width:
        Console.info('{} {} [{}]'.format(
            message, '.' * (width - len(message)), status
        ))
    else:
        Console.info('{} [{}]'.format(message, status))


@_extend(Console, 'ok')
def _console_ok(message, columns=79):
    """Display an OK message.

    Execution step ............................... [PASSED]

    :param str message: the message to display
    :param int columns: optional maximum column to use for display (default to
        79)
    """
    Console._status(message, 'PASSED', columns)


@_extend(Console, 'ko')
def _console_ko(message, columns=79):
    """Display a KO message.

    Execution step ............................... [FAILED]

    :param str message: the message to display
    :param int columns: optional maximum column to use for display (default to
        79)
    """
    Console._status(message, 'FAILED', columns)


class Plugin(object):

    """GNAThub plugin interface.

    A plugin is a Python class that describe how to configure, run and collect
    data output by an external tool.

    Each plugin should be dedicated to only one tool.

    To implement a now plugin, simply creates a new Python class inheriting
    from this (:class:`GNAThub.Plugin`) abstract base class.

    All plugins are collected using the inheritance mechanism, i.e. the GNAThub
    driver will automatically find all classes implementing the
    :class:`GNAThub.Plugin` interface. No manual registration needed.
    """

    __metaclass__ = ABCMeta

    def __init__(self):
        """Initialize instance properties."""
        # A custom instance of a logger. It is implemented in Ada and based on
        # the GNATCOLL.Traces module.
        self.log = logging.getLogger(self.name or self.__class__.__name__)

        # The execution status of this plugin. Initialized to NOT_EXECUTED.
        # Should be set to EXEC_FAILURE or EXEC_SUCCESS by the plugin in the
        # Plugin.execute method.
        self._exec_status = NOT_EXECUTED

    @property
    def name(self):
        """Return the name of the tool.

        :return: the tool name
        :rtype: str
        """
        return type(self).__name__.lower()

    def info(self, message, *args):
        """Display an informative message, prefixed with the plug-in name.

        :param str message: the message to display
        :param collections.Iterable args: format string arguments
        """
        Console.info(message % args, prefix=self.name)

    def warn(self, message, *args):
        """Display a warning message, prefixed with the plug-in name.

        :param str message: the message to display
        :param collections.Iterable args: format string arguments
        """
        Console.warn(message % args, prefix=self.name)

    def error(self, message, *args):
        """Display an error message, prefixed with the plug-in name.

        :param str message: the message to display
        :param collections.Iterable args: format string arguments
        """
        Console.error(message % args, prefix=self.name)

    def setup(self):
        """Called prior to a call to :meth:`Plugin.execute`.

        This is where environment setup should be done to ensure a correct
        execution of the tool.
        """
        pass

    def teardown(self):
        """Called after a call to :meth:`Plugin.execute`.

        This is where environment cleanup should be done to ensure a consistent
        state for a future execution.
        """
        pass

    @property
    def exec_status(self):
        """Return the execution status for the tool.

        Can be one of the following:

        * :command:`GNAThub.NOT_EXECUTED`: plugin did not run yet
        * :command:`GNAThub.EXEC_FAILURE`: an error occurred during the plugin
          execution
        * :command:`GNAThub.EXEC_SUCCESS`: the plugin execution completed
          successfully

        :return: the exit code
        :rtype: int
        """
        return self._exec_status

    @exec_status.setter
    def exec_status(self, status):
        """Set the execution status for the tool.

        Can be one of the following:

        * :command:`GNAThub.NOT_EXECUTED`: plugin did not run yet
        * :command:`GNAThub.EXEC_FAILURE`: an error occurred during the plugin
          execution
        * :command:`GNAThub.EXEC_SUCCESS`: the plugin execution completed
          successfully

        :param int status: the new execution status
        """
        if status not in (EXEC_FAILURE, EXEC_SUCCESS, NOT_EXECUTED):
            raise Error('invalid execution status code')
        self._exec_status = status


class Runner(object):
    """Plugin extension point for analyser tools."""

    __metaclass__ = ABCMeta

    @abstractmethod
    def run(self):
        """Abstract method. Needs custom implementation by derived classes.

        Execute the external tool. This method is called after :meth:`setup`
        and before :meth:`teardown`.

        :return: GNAThub.EXEC_SUCCESS on success, GNAThub.EXEC_FAILURE
            otherwise
        :rtype: int
        """
        pass


class Reporter(object):
    """Plugin extension point for reporter tools."""

    __metaclass__ = ABCMeta

    @abstractmethod
    def report(self):
        """Abstract method. Needs custom implementation by derived classes.

        Collect the results produced by the external tool. This method is
        called after :meth:`setup` and before :meth:`teardown`.

        :return: GNAThub.EXEC_SUCCESS on success, GNAThub.EXEC_FAILURE
            otherwise
        :rtype: int
        """
        pass


class ToolArgsPlaceholder(object):
    """Placeholder for tool-specific arguments.

    By default tool-specific arguments (see :func:`tool_args`) are
    automatically appended to the end of the command line by :class:`Run`. In
    some cases this is not flexible enough (eg. when used in combination of the
    :command:`-output-msg[-only]` switches of :prog:`codepeer`. For such cases,
    this placeholder object should be inserted at the correct position in the
    command line so that it can be substituted by the appropriate list of
    arguments.
    """

    def __init__(self, tool_name):
        self.tool_name = tool_name


class Run(object):

    """Class to handle processes."""

    def __init__(self, name, argv, env=None, workdir=None, out=None,
                 capture_stderr=True):
        """Spawn the process.

        Use subprocess.Popen to spawn a process and returns its exit code.

        :param str name: the name of the executable
        :param collections.Iterable[str] argv: the argument array
        :param dict[str, str] env: Map containing the environment to pass
            through to the process. If ``None``, ``os.environ`` is used.
        :param str workdir: the directory in which to execute the process. If
            ``None``, use the current directory.
        :param str out: the log file to use
        :param bool capture_stderr: whether to capture the standard error
            in the log file
        """
        self.name = name
        self.argv = self.expand_argv(name, argv)
        self.status = 127
        self.pid = -1
        self.out = out
        self.log = logging.getLogger(self.__class__.__name__)

        self.log.debug(
            'cd %s; %s', workdir if workdir is not None else os.getcwd(),
            self.cmdline_image())

        if verbose():
            Console.info(self.cmdline_image())

        try:
            with open(self.output(), 'w') as output:
                self.inferior = Popen(
                    self.argv, env=env, stdin=None, stdout=output,
                    stderr=STDOUT if capture_stderr else None,
                    cwd=workdir)

                Console.info('output redirected to %s' % output.name)
                self.pid = self.inferior.pid
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
        """Wait until process ends and returns its status."""
        self.status = self.inferior.wait()
        return self.status

    @staticmethod
    def expand_argv(name, argv):
        """TODO(delay)

        :param str name: the name of the executable
        :param collections.Iterable[str] argv: the argument array
        """
        has_placeholder, expanded_argv = False, []

        for arg in list(argv):
            if isinstance(arg, ToolArgsPlaceholder):
                has_placeholder = True
                expanded_argv += tool_args(arg.tool_name)
            else:
                expanded_argv.append(arg)

        if not has_placeholder:
            expanded_argv += tool_args(name)

        return expanded_argv

    @staticmethod
    def quote(arg):
        """Return the quoted version of the given argument.

        :param str arg: the argument to quote
        :return: the quoted argument
        :rtype: str
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
        """Return a string image of the given command.

        :return: the command line image
        :rtype: str
        """
        return ' '.join((Run.quote(arg) for arg in self.argv))

    def output(self):
        """Return the path to the output file.

        :return: the full path to the file
        :rtype: str
        """
        return self.out or os.path.join(logs(), self.name + '.log')
