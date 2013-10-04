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

__all__ = ['dao', 'utils']

from sqlalchemy.orm import sessionmaker
Session = sessionmaker()

import GNAThub
import GPS
import os
import random

import GNAThubCore
from GNAThubCore import *

from abc import ABCMeta, abstractmethod

from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement, ParseError

(EXEC_FAIL, EXEC_SUCCESS, PROCESS_NOT_LAUNCHED) = range(3)


class Plugin(object):
    __metaclass__ = ABCMeta

    LOG_FILE_NAME = 'gnathub-tool-%d.log' % random.randint(1, 100)

    def __init__(self, name):
        self.name = name

    def setup(self):
        pass

    @abstractmethod
    def execute(self):
        pass

    def teardown(self):
        pass

    @classmethod
    def get_log_file_path(cls):
        """Class method that returns full path for the plugin logging file"""
        path = os.path.join(GNAThub.utils.get_project_obj_dir(),
                            GNAThub.logs(), cls.LOG_FILE_NAME)
        return path


class GPSTarget(object):
    # Value updated by GNAThub.utils.OutputParser
    EXECUTION_SUCCES = PROCESS_NOT_LAUNCHED
    GNAT = """%attr(ide'gnat,gnat)"""
    OBJ_DIR = '%O'
    PRJ_FILE = '%pp'

    def __init__(self, name, output_parser, cmd_args=[]):
        self.name = name
        self.cmdline = cmd_args
        self.output_parser = output_parser
        # Re-initialise execution status, as tool execution is sequantiel
        GPSTarget.EXECUTION_SUCCESS = PROCESS_NOT_LAUNCHED

        if not self.cmdline:
            Log.warn('Missing command line for: %s' % self.name)

    def __prettify(self, elem):
        """Returns a pretty-printed XML string for the Element."""

        raw = ElementTree.tostring(elem, 'utf-8')
        reparsed = minidom.parseString(raw)

        return reparsed.toxml()

    def __build_GPS_target(self):
        # GPS Root
        root_xml = Element('GPS')

        # Builder mode
        builder_mode_xml = SubElement(root_xml, 'builder-mode',
                                      {'name' : 'default'})
        builder_desc_xml = SubElement(builder_mode_xml, 'description')
        builder_desc_xml.text = 'Build with default switches defined in the project'

        # Target model
        model_xml = SubElement(root_xml, 'target-model',
                               {'name' : 'gnathub'})

        model_desc_xml = SubElement(model_xml, 'description')
        model_desc_xml.text = 'Generic target model for GNAThub'

        # Target
        target_xml = SubElement(root_xml, 'target',
                                {'model'    : model_xml.attrib['name'],
                                 'category' : 'default',
                                 'name'     : self.name})
        # Command line
        target_cmd_xml = SubElement(target_xml, 'command-line')

        for arg in self.cmdline:
            SubElement(target_cmd_xml, 'arg').text = arg

        # Output parsers
        output_parser_xml = SubElement(target_xml, 'output-parsers')
        output_parser_xml.text = '%s output_collector' % self.output_parser

        return self.__prettify(root_xml)

    def execute(self):
        Log.debug('Executing %s with comand line: %s' % (self.name, ' '.join(self.cmdline)))
        gps_target_xml = self.__build_GPS_target()

        Log.debug('XML base for GPS target:\n%s' % gps_target_xml)
        GPS.parse_xml(gps_target_xml)

        Log.debug('Building target: %s' % self.name)
        target = GPS.BuildTarget(self.name)
        target.execute()

        Log.debug('GPSTarget.EXECUTION_SUCCESS = %s' %
                  str(GPSTarget.EXECUTION_SUCCESS))

        return GPSTarget.EXECUTION_SUCCESS
