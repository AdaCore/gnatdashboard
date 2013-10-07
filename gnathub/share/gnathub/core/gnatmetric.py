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

import os
import GPS
import GNAThub

from GNAThub import GPSTarget, Log
from GNAThub import utils
from GNAThub import Session, dao, db
from GNAThub.db import Rule, Message
from GNAThub.utils import OutputParser, create_parser

from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError

## GnatmetricOutputParser #####################################################
##
class GnatmetricOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Gnatmetric.get_log_file_path(), 'w+a') as log:
            log.write(text)

    def on_stderr(self,text):
        with open (Gnatmetric.get_log_file_path(), 'w+a') as log:
            log.write(text)

## Gnatmetric ################################################################
##
class Gnatmetric(GNAThub.Plugin):
    """GNATmetric plugin for GNAThub

       Launch GNATmetric
    """
    LOG_FILE_NAME='gnatmetric.log'
    OUTPUT_FILE_NAME='metrix.xml'

    def __init__ (self, session):
        super(Gnatmetric, self).__init__('GNATmetric')
        self.session = session
        # Create Gnat Metric Tool
        self.process = GPSTarget(name=self.name,
                                 output_parser='gnatmetricoutputparser',
                                 cmd_args=self.__cmd_line())

    def __cmd_line(self):
        """Create Gnat Metric command line argument list for GPS target
           Return:
               - list of command line argument for GPSTarget
        """
        out_file = '%s/%s' % (GPSTarget.OBJ_DIR, self.OUTPUT_FILE_NAME)
        prj_file = '-P%s' % GPSTarget.PRJ_FILE
        return [GPSTarget.GNAT, 'metric', '-ox', out_file, prj_file, '-U']

    def parse_metrix_xml_file (self):
        """Parse GNATmetric xml report and save data to the DB
           Return:
               - GNAThub.EXEC_SUCCESS: if transaction have been comitted to DB
               - GNAThub.EXEC_FAIL: if error happened while parsing the xml
                                   report
        """
        tool = dao.save_tool(self.session, self.name)

        xml_report = os.path.join(utils.get_project_obj_dir(),
                                   self.OUTPUT_FILE_NAME)
        try:
            tree = ElementTree.parse(xml_report)

            # Fetch all files
            for file_node in tree.findall('./file'):
                file = dao.get_file(self.session, file_node.attrib.get('name'))
                # Save file level metrics
                if file:
                    for metric in file_node.findall('./metric'):
                        rule = dao.get_or_create_rule(self.session, tool,
                                                      db.METRIC_KIND,
                                                      metric.attrib.get('name'))
                        file.messages.append(Message(metric.text, rule))
                else:
                    Log.warn ('File not found, skipping all messages from: %s' %
                              file_node.attrib.get('name'))
                    continue

                # Save unit level metric
                for unit in file_node.findall('.//unit'):
                    for metric in unit.findall('./metric'):
                        pass
                        # /!\ Not handle for now: to be done /!\
                        # File --> file_node.attrib.get('name'),
                        # Entity Line --> unit.attrib.get('line'),
                        # Entity name --> unit.attrib.get('name'),
                        # Entity Col --> unit.attrib.get('col'),
                        # Metric name --> metric.attrib.get('name'),
                        # Metric value --> metric.text)

            self.session.commit()
            return GNAThub.EXEC_SUCCESS

        except ParseError:
            Log.fatal('Unable to parse gnat metric xml report')
            Log.fatal('%s:%s:%s - :%s' % (e.filename, e.lineno, e.text, e.msg))
            return GNAThub.EXEC_FAIL

        except IOError as e:
            Log.fatal(e)
            return GNAThub.EXEC_FAIL

    def execute(self):
        status = self.process.execute()

        # If GNATmetric execution has failed
        if status == GNAThub.EXEC_FAIL:
                Log.warn('GNAT Metric execution returned on failure')
                Log.warn('For more details, see log file: %s' % self.get_log_file_path())
        # Just return status if GNATmetrics have not been launched
        elif status == GNAThub.PROCESS_NOT_LAUNCHED:
            return status
        # If GNATmetric succeed: parse xml report and save data to DB
        return self.parse_metrix_xml_file()

#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
