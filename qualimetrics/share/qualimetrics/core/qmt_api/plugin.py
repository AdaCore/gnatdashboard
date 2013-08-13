#########################################
#        Qualimetrics Plugin API        #
#########################################

import os
import utils
import Qmt
import GPS
import random
import logging
from abc import ABCMeta, abstractmethod
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement, ParseError
from xml.dom import minidom

logger = logging.getLogger(__name__)
(EXEC_FAIL, EXEC_SUCCESS, PROCESS_NOT_LAUNCHED) = range(3)

## Plugin ######################################################
##
class Plugin:
    __metaclass__ = ABCMeta
    # A clear mention need to be done in documentation about the log file
    # name for tool and the declaration of a custom output parser
    # in plugin script which is mandatory for the consistency of
    # tool execution information.
    LOG_FILE_NAME='qmt-tool-%d.log' % random.randint(1,100)

    def __init__(self, name):
        self.name = name

    def setup(self):
        return

    @abstractmethod
    def execute(self):
        return

    def teardown(self):
        return

    @classmethod
    def get_log_file_path(cls):
        """Class method that returns full path for the plugin logging file"""
        path = os.path.join(utils.get_project_obj_dir(),
                            Qmt.logs_dir(), cls.LOG_FILE_NAME)
        return path

## GPSTarget ##################################################################
##
class GPSTarget(object):
    # Value updated by utils.OutputParser
    EXECUTION_SUCCES=PROCESS_NOT_LAUNCHED
    GNAT="""%attr(ide'gnat,gnat)"""
    OBJ_DIR='%O'
    PRJ_FILE='%pp'

    def __init__(self, name, output_parser, cmd_args=[]):
        self.name = name
        self.cmd_line = cmd_args
        self.output_parser = output_parser
        # Re-initialise execution status, as tool execution is sequantiel
        GPSTarget.EXECUTION_SUCCESS=PROCESS_NOT_LAUNCHED
        if not len(self.cmd_line):
            logging.warn('Command line have not been specified when creating the tool: %s' % self.name)

    def __prettify(self, elem):
        """Return a pretty-printed XML string for the Element. """
        rough_string = ElementTree.tostring(elem, 'utf-8')
        reparsed = minidom.parseString(rough_string)
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
                               {'name' : 'qualimetrics'})

        model_desc_xml = SubElement(model_xml, 'description')
        model_desc_xml.text = 'Generic target model for Qualimetrics'
        # Target
        target_xml = SubElement(root_xml, 'target',
                                {'model'    : model_xml.attrib['name'],
                                 'category' : 'default',
                                 'name'     : self.name})
        # Command line
        target_cmd_xml = SubElement(target_xml, 'command-line')
        for arg in self.cmd_line:
            SubElement(target_cmd_xml, 'arg').text = arg
        # Output parsers
        output_parser_xml = SubElement(target_xml, 'output-parsers')
        output_parser_xml.text = '%s output_collector' % self.output_parser
        return self.__prettify(root_xml)

    def execute(self):
        logging.debug('Executing %s with comand line: %s' % (self.name, ' '.join(self.cmd_line)))
        gps_target_xml = self.__build_GPS_target()
        logging.debug('xml base for GPS target:\n%s' % gps_target_xml)
        GPS.parse_xml(gps_target_xml)
        logger.debug('Building target: %s' % self.name)
        target = GPS.BuildTarget(self.name)
        target.execute()
        logging.debug('Status of GPSTarget.EXECUTION_SUCCESS: %s' % str(GPSTarget.EXECUTION_SUCCESS))
        return GPSTarget.EXECUTION_SUCCESS


