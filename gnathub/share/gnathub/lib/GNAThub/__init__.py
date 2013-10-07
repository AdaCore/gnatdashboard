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

from sqlalchemy.orm import sessionmaker
SESSION = sessionmaker()

import GNAThubCore
import GPS
import os
import tempfile

# pylint: disable=W0401, W0622
from GNAThubCore import *       # NOQA (disable warning from flake8)

from abc import ABCMeta, abstractmethod

from xml.dom.minidom import getDOMImplementation as dom

EXEC_FAIL, EXEC_SUCCESS, PROCESS_NOT_LAUNCHED = range(3)


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
        pass

    def setup(self):
        """This method is called prior to any call to Plugin.execute.

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

        This is where environment cleanup should be done to ensure a consistant
        state for a future execution.
        """
        pass

    @property
    def name(self):
        """Returns the name of the tool, as specified by the TOOL_NAME class
        variable.

        RETURNS
            The name of the tool
            :rtype: a string
        """

        return self.TOOL_NAME

    @classmethod
    def logs(cls):
        """Returns the path to the file that contains the logs for this tool's
        execution.

        RETURNS
            The absolute path to the log file
            :rtype: a string
        """

        if cls.LOG_FILE is None:
            _, path = tempfile.mkstemp(prefix='%s-' %cls.TOOL_NAME.lower(),
                                       text=True, suffix='.log',
                                       dir=GNAThubCore.logs())
            cls.LOG_FILE = path

        return cls.LOG_FILE


class GPSTarget(object):
    """Defines a GPS target enironment.

    ???
    """

    # Value updated by GNAThub.utils.OutputParser
    EXECUTION_SUCCES = PROCESS_NOT_LAUNCHED
    GNAT = """%attr(ide'gnat,gnat)"""
    OBJ_DIR = '%O'
    PRJ_FILE = '%pp'

    def __init__(self, name, output_parser, cmd_args=None):
        self.name = name
        self.cmdline = cmd_args if cmd_args is not None else []
        self.parser = output_parser

        # Re-initialise execution status, as tool execution is sequantiel
        GPSTarget.EXECUTION_SUCCESS = PROCESS_NOT_LAUNCHED

        if not self.cmdline:
            GNAThubCore.Log.warn('Missing command line for: %s' % self.name)

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
        root = document.documentElement

        # Builder mode
        builder_mode = document.createElement('builder-mode')
        builder_mode.setAttribute('name', 'default')

        builder_mode_desc = document.createElement('description')
        builder_mode_desc.appendChild(_text('Inherit switches from project'))

        builder_mode.appendChild(builder_mode_desc)
        root.appendChild(builder_mode)

        # Target model
        target_model = document.createElement('target-model')
        target_model.setAttribute('name', 'gnathub')

        target_model_desc = document.createElement('description')
        target_model_desc.appendChild(_text('Generic GNAThub target model'))

        target_model.appendChild(target_model_desc)
        root.appendChild(target_model)

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
        root.appendChild(target)

        return document.toprettyxml()

    def execute(self):
        """Executes the target."""

        GNAThubCore.Log.debug('Executing %s: %s' % (self.name,
                                                    ' '.join(self.cmdline)))

        xml = self.__build_gps_target()

        GNAThubCore.Log.debug('GPSTarget XML:%s%s' % (os.linesep, xml))
        GPS.parse_xml(xml)

        GNAThubCore.Log.debug('Building target: %s' % self.name)
        target = GPS.BuildTarget(self.name)
        target.execute()

        GNAThubCore.Log.debug('GPSTarget.EXECUTION_SUCCESS = %s' %
                              str(GPSTarget.EXECUTION_SUCCESS))

        return GPSTarget.EXECUTION_SUCCESS
