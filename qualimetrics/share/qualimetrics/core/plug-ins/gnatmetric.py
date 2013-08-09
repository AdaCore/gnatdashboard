import os
import GPS
import logging
import qmt_api
from qmt_api import plugin
from qmt_api import utils
from qmt_api import Session, dao, db
from qmt_api.db import Rule, Message
from qmt_api.utils import OutputParser, create_parser
from qmt_api.plugin import GPSTarget, Plugin
from xml.etree import ElementTree
from xml.etree.ElementTree import ParseError

logger = logging.getLogger(__name__)

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
class Gnatmetric(Plugin):
    """GNATmetric plugin for qualimetrics

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
               - plugin.EXEC_SUCCESS: if transaction have been comitted to DB
               - plugin.EXEC_FAIL: if error happened while parsing the xml
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
                        rule = dao.get_or_create_rule(self.session,
                                                     tool,
                                                     db.METRIC_KIND,
                                                     metric.attrib.get('name'))
                        file.messages.append(Message(metric.text, rule))
                else:
                    logging.warn
                    ('File not found, skipping all messages for file: %s' % \
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
            return plugin.EXEC_SUCCESS

        except ParseError:
            logger.error('Unable to parse gnat metric xml report')
            logger.error('%s:%s:%s - :%s' % (e.filename, e.lineno,
                                             e.text, e.msg))
            return plugin.EXEC_FAIL

        except IOError as e:
            logger.error(e)
            return plugin.EXEC_FAIL

    def execute(self):
        status = self.process.execute()

        # If GNATmetric execution has failed
        if status == plugin.EXEC_FAIL:
                logging.warn('GNAT Metric execution returned on failure')
                logging.warn('For more details, see log file: %s' % self.get_log_file_path())
        # Just return status if GNATmetrics have not been launched
        elif status == plugin.PROCESS_NOT_LAUNCHED:
            return status
        # If GNATmetric succeed: parse xml report and save data to DB
        return self.parse_metrix_xml_file()

#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
