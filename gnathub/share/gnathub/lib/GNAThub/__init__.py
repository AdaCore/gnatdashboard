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

"""
This module defines the core components of GNAThub plugin mechanism:

    - The GNAThub.Plugin Abstract Base Class
    - The GPSTarget Helper Class
"""


def root():
    """Returns the path to the GNAThub-specific root directory.
    Usually:

        <project_object_dir>/gnathub

    RETURNS
        :rtype: a path as a string
    """

    pass        # Implemented in Ada


def logs():
    """Returns the path to the GNAThub-specific directory for logs.
    Usually:

        <project_object_dir>/gnathub/logs

    RETURNS
        :rtype: a path as a string
    """

    pass        # Implemented in Ada


def database():
    """Returns the path to the GNAThub SQLite database.
    Usually:

        <project_object_dir>/gnathub/gnathub.db

    RETURNS
        :rtype: a path as a string
    """

    pass        # Implemented in Ada


class Log(object):
    """A logger object. Fully implemented in Ada."""

    def __init__(self):
        """Instance constructor."""

        raise Error('GNAThub.Log must not be instanciated')

    @staticmethod
    def info(message):
        """Prints an informative message. Activated at default verbosity
        output.
        """

        pass    # Implemented in Ada

    @staticmethod
    def warn(message):
        """Prints a warning message. Activated at default verbosity output."""

        pass    # Implemented in Ada

    @staticmethod
    def error(message):
        """Prints an error message. Always activated."""

        pass    # Implemented in Ada

    @staticmethod
    def fatal(message):
        """Prints a fatal message. Always activated."""

        pass    # Implemented in Ada

    @staticmethod
    def debug(message):
        """Prints a debug message. Activated at higher verbosity level."""

        pass    # Implemented in Ada

    @staticmethod
    def progress(current, natural, new_line=False):
        """Prints a progess message. Activated at default verbosity level.  If
        new_line is True, then terminate the line with a '\n' character.
        Defaults to False.
        """

        pass    # Implemented in Ada


# Install all Ada extensions, i.e. functions and classes implemented in Ada and
# exported to Python. These extensions should be declared above this statement
# with no implementation.

# pylint: disable=W0401, W0622, F0401
from GNAThubCore import *       # NOQA (disable warning from flake8)

# Now that all Ada extensions have been planted into this module, we can
# define pure-Python extensions.

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

__DB_ENGINE = create_engine('sqlite:///%s' % database(), echo=False)
_DB_SESSION_FACTORY = sessionmaker()
_DB_SESSION_FACTORY.configure(bind=__DB_ENGINE)

import os
import tempfile

from abc import ABCMeta, abstractmethod
from twisted.internet import protocol, reactor, threads

EXEC_FAIL, EXEC_SUCCESS, NOT_EXECUTED = range(3)


class Error(Exception):
    """Base class for exceptions in this module."""
    pass


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

    TOOL_NAME = None
    LOG_FILE = None

    def __init__(self):
        """Instance constructor."""

        self._exec_status = NOT_EXECUTED
        self.session = None

    def display_command_line(self):
        """This method returns a list similar to argv. However, this command
        line does not need to be functional and its only purpose is to be
        printed on the GNAThub tool output.

        RETURNS
            :rtype: a list of string
        """

        return [self.name.lower()]

    def setup(self):
        """This method is called prior to a call to Plugin.execute.

        This is where environment setup should be done to ensure a correct
        execution of the tool.
        """

        self.session = _DB_SESSION_FACTORY()

    @abstractmethod
    def execute(self):
        """Abstract method. Need implementation.

        Executes the external tool. This method is called after setup() and
        before teardown().
        """

        pass

    def teardown(self):
        """This method is called after a call to Plugin.execute.

        This is where environment cleanup should be done to ensure a consistant
        state for a future execution.
        """

        if self.session:
            self.session.close()

    def ensure_chain_reaction(self, async=False):
        """This method is called after a call to Plugin.execute.

        This is where environment cleanup should be done to ensure a consistant
        state for a future execution.

        PARAMETERS
            :param async: whether to have this call run synchronously or
                asynchronously. This is particularly useful when a plug-in's
                execution does not require asynchronous processing (e.g. does
                not spawn a process) and thus needs to defer the execution of
                this function.
            :type async: a boolean.
        """

        if async:
            threads.deferToThread(_chain_reaction)
        else:
            _chain_reaction()

    @property
    def exec_status(self):
        """Returns the execution status for the tool.

        Can be one of the following:
            GNAThub.NOT_EXECUTED: plugin did not run yet
            GNAThub.EXEC_FAIL: an error occured during the plugin execution
            GNAThub.EXEC_SUCCESS: the plugin execution completed successfully

        RETURNS
            :rtype: a number.
        """

        return self._exec_status

    # pylint: disable=E1101, E0102
    @exec_status.setter
    def exec_status(self, status):
        """Sets the execution status for the tool.

        Can be one of the following:
            GNAThub.NOT_EXECUTED: plugin did not run yet
            GNAThub.EXEC_FAIL: an error occured during the plugin execution
            GNAThub.EXEC_SUCCESS: the plugin execution completed successfully

        PARAMETERS
            :param status: the new execution status.
            :type status: a number.
        """

        if status not in (EXEC_FAIL, EXEC_SUCCESS, NOT_EXECUTED):
            raise Error('invalid execution code')

        self._exec_status = status

    @property
    def name(self):
        """Returns the name of the tool, as specified by the TOOL_NAME class
        variable.

        RETURNS
            :rtype: a string.
        """

        return self.TOOL_NAME

    @classmethod
    def logs(cls):
        """Returns absolute the path to the file that contains the logs for
        this tool's execution.

        RETURNS
            :rtype: a string
        """

        tool = '-'.join(cls.TOOL_NAME.lower().split())

        if cls.LOG_FILE is None:
            _, path = tempfile.mkstemp(prefix='%s-' % tool, text=True,
                                       suffix='.log', dir=logs())
            cls.LOG_FILE = path

        return cls.LOG_FILE


class ProcessProtocol(protocol.ProcessProtocol):
    """The ProcessProtocol passed to twisted.internet.reactor.spawnProcess is
    the interaction with the process. It has several methods to deal with
    events specific to a process.
    """

    def __init__(self, plugin):
        """Instance constructor.

        PARAMETERS
            :param plugin: A plugin object.
            :type plugin: An object derived from GNAThub.Plugin.
        """

        self.plugin = plugin
        self.exit_code = None

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def connectionMade(self):
        """This is called when the program is started, and makes a good place
        to write data into the stdin pipe (using self.transport.write).
        """

        Log.debug('%s: process started' % self.plugin.name)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def outReceived(self, data):
        """This is called with data that was received from the process' stdout
        pipe.

        PARAMETERS
            :param data: data that was received from the process' stdout pipe.
            :type data: a string.
        """

        pass

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def errReceived(self, data):
        """This is called with data from the process' stderr pipe. It behaves
        just like outReceived.

        PARAMETERS
            :param data: data that was received from the process' stderr pipe.
            :type data: a string.
        """

        pass

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def processExited(self, reason):
        """This is called when the child process has been reaped, and receives
        information about the process' exit status.

        PARAMETERS
            :param reason: The status is passed in the form of a Failure
                instance, created with a .value that either holds a ProcessDone
                object if the process terminated normally (it died of natural
                causes instead of receiving a signal, and if the exit code was
                0), or a ProcessTerminated object (with an .exitCode attribute)
                if something went wrong.
            :type reason: twisted.python.failure.Failure object.
        """

        Log.debug('%s: exited: %s' % (self.plugin.name, reason.value.__dict__))

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def processEnded(self, reason):
        """This is called when all the file descriptors associated with the
        child process have been closed and the process has been reaped. This
        means it is the last callback which will be made onto a
        ProcessProtocol.

        PARAMETERS
            :param reason: The status parameter has the same meaning as it does
                for processExited.
            :type reason: twisted.python.failure.Failure object.
        """

        Log.debug('%s: terminated with status: %d' % (self.plugin.name,
                                                      reason.value.exitCode))
        self.exit_code = reason.value.exitCode


class LoggerProcessProtocol(ProcessProtocol):
    """A simple ProcessProtocol that logs both standard and error output to a
    file.
    """

    def __init__(self, plugin):
        """Instance constructor.

        PARAMETERS
            :param plugin: The plugin class object. This class must implement
                the GNAThub.Plugin Abstract Base Class.
            :type plugin: a GNAThub.Plugin object.
        """

        ProcessProtocol.__init__(self, plugin)

        Log.debug('%s: will log to: %s' % (plugin.name, plugin.logs()))

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def outReceived(self, data):
        """Inherited."""

        ProcessProtocol.outReceived(self, data)

        with open(self.plugin.logs(), 'w+a') as log:
            log.write(data)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def errReceived(self, data):
        """Inherited."""

        ProcessProtocol.errReceived(self, data)

        with open(self.plugin.logs(), 'w+a') as log:
            log.write(data)


class Process(object):
    """An asynchronous process abstraction class."""

    def __init__(self, name, argv, process_protocol=None):
        """Instance constructor."""

        self.name = name
        self.argv = argv
        self.protocol = process_protocol
        self.exit_code = None

        if self.protocol is None:
            self.protocol = ProcessProtocol(self.name)

    def execute(self, env=None, workdir=None):
        """Spawnes the process, run the Twisted's Reactor and returns the
        process exit code.

        PARAMETERS
            :param env: dictionary containing the environment to pass through
                to the process. If None, os.environ is used.
            :type env: a dictionary.
            :param workdir: the directory in which to execute the process. If
                None, use the current directory.
            :type workdir: a string.

        RETURNS
            :rtype: a number
        """

        Log.debug('%s: spawning process' % self.name)
        Log.debug(os.linesep.join(['Arg = %s' % arg for arg in self.argv]))

        environ = env if env is not None else os.environ

        # pylint: disable=E1101
        # Disable "Module {} has no member {}" error
        reactor.spawnProcess(self.protocol, self.argv[0], self.argv,
                             env=environ, path=workdir)


# A chain of plugin in the order in which they are registered through the
# GNAThub.register() function.
_PLUGINS = []


def register(plugin):
    """Registers a plugin.

    A plugin must implement the GNAThub.Plugin Abstract Base Class. All
    registered plugins are chained for sequencial execution.

    PARAMETERS
        :param plugin: The plugin to register.
        :type plugin: An object derived from GNAThub.Plugin.
    """

    _PLUGINS.append(plugin)


def _chain_reaction():
    """???"""

    assert _PLUGINS
    current = _PLUGINS.pop(0)
    current.teardown()

    _try_chain_reaction()


def _try_chain_reaction():
    """???"""

    if not _PLUGINS:
        abort()
        return False

    plugin = _PLUGINS[0]

    Log.debug('%s: running plugin' % plugin.name)
    Log.info(' '.join(plugin.display_command_line()))

    plugin.setup()
    plugin.execute()

    return True


def run():
    """???"""

    if not _PLUGINS:
        Log.info('gnathub: nothing to do.')
        return

    if _try_chain_reaction():
        Log.debug('Starting reactor...')
        # pylint: disable=E1101
        # Disable "Module {} has no member {}" error
        reactor.run()


def abort():
    """???"""

    # pylint: disable=E1101
    # Disable "Module {} has no member {}" error
    if reactor.running:
        Log.debug('Terminating reactor')
        # pylint: disable=E1101
        # Disable "Module {} has no member {}" error
        reactor.stop()


# pylint: disable=F0401
# Disable "Unable to import" error
import GPS

from xml.dom.minidom import getDOMImplementation as dom


class GPSTarget(object):
    """Defines a GPS target enironment.

    ???
    """

    # Value updated by GNAThub.utils.OutputParser
    EXECUTION_SUCCES = NOT_EXECUTED
    GNAT = """%attr(ide'gnat,gnat)"""
    OBJ_DIR = '%O'
    PRJ_FILE = '%pp'

    def __init__(self, name, output_parser, cmd_args=None):
        self.name = name
        self.cmdline = cmd_args if cmd_args is not None else []
        self.parser = output_parser

        # Re-initialise execution status, as tool execution is sequantiel
        GPSTarget.EXECUTION_SUCCESS = NOT_EXECUTED

        if not self.cmdline:
            Log.warn('Missing command line for: %s' % self.name)

    def __build_gps_target(self):
        """Creates the XML document describing the GPS Target.

        RETURNS
            :rtype: the XML document as a string
        """

        # XML document
        document = dom().createDocument(None, "GPS", None)

        def _text(data):
            """Returns an XML Text Node whose content is data.

            RETURNS
                :rtype: xml.dom.Text node
            """

            return document.createTextNode(data)

        # GPS Root
        doc = document.documentElement

        # Builder mode
        builder_mode = document.createElement('builder-mode')
        builder_mode.setAttribute('name', 'default')

        builder_mode_desc = document.createElement('description')
        builder_mode_desc.appendChild(_text('Inherit switches from project'))

        builder_mode.appendChild(builder_mode_desc)
        doc.appendChild(builder_mode)

        # Target model
        target_model = document.createElement('target-model')
        target_model.setAttribute('name', 'gnathub')

        target_model_desc = document.createElement('description')
        target_model_desc.appendChild(_text('Generic GNAThub target model'))

        target_model.appendChild(target_model_desc)
        doc.appendChild(target_model)

        # Target
        target = document.createElement('target')
        target.setAttribute('model', target_model.getAttribute('name'))
        target.setAttribute('category', 'default')
        target.setAttribute('name', self.name)

        # Command line
        cmdline = document.createElement('command-line')

        for element in self.cmdline:
            arg = document.createElement('arg')
            arg.appendChild(_text(element))
            cmdline.appendChild(arg)

        target.appendChild(cmdline)

        # Output parsers
        output_parsers = document.createElement('output-parsers')
        output_parsers.appendChild(_text('%s output_collector' % self.parser))

        target.appendChild(output_parsers)
        doc.appendChild(target)

        return document.toprettyxml()

    def execute(self):
        """Executes the target."""

        Log.debug('Executing %s: %s' % (self.name, ' '.join(self.cmdline)))

        xml = self.__build_gps_target()

        Log.debug('GPSTarget XML:%s%s' % (os.linesep, xml))
        GPS.parse_xml(xml)

        Log.debug('Building target: %s' % self.name)
        target = GPS.BuildTarget(self.name)

        Log.debug('Executing target: %s' % self.name)
        target.execute()

        Log.debug('GPSTarget.EXECUTION_SUCCESS = %s' %
                  str(GPSTarget.EXECUTION_SUCCESS))

        return GPSTarget.EXECUTION_SUCCESS
