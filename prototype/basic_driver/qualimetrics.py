#! /usr/bin/env python

## Imports #################################################################
##
import logging
import os
import sys
import os.path
import ConfigParser
import argparse
import json
import shutil
from subprocess import call, Popen, PIPE, STDOUT
from ConfigParser import NoOptionError

##################
#    CONSTANTS   #
##################
QMT_CONF_FILE = 'qualimetrics.cfg'
# Default Qualimetrics configuration
BASE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
CURRENT_DIR = os.getcwd()
DEFAULT_CONFIG = {'qualimetrics.reportDir.path': 'reports',
                  'gcov.outputDir.path'        : None}

## Tool #########################################################
##
class Tool:
    def __init__(self, name, script_name, report_name, qmt_key, sonar_key):
        self.name = name
        self.script_name = script_name
        self.report_name = report_name
        self.qmt_key = qmt_key
        self.sonar_key = sonar_key

## SonarConfiguration #########################################################
##
class SonarConfiguration(object):
    MAIN_SECTION = 'Sonar'

    def __init__(self):
        # Set output file path
        self.output_file = os.path.join(CURRENT_DIR, 'sonar-project.properties')
        # Create a configuration
        self.configuration = ConfigParser.ConfigParser()
        self.configuration.add_section(self.MAIN_SECTION)
        # Enable case sensitive for key
        self.configuration.optionxform = str
        # Initialise configuration with default values
        self.configuration. set(self.MAIN_SECTION, 'sonar.language', 'ada')
        self.configuration. set(self.MAIN_SECTION, 'sonar.sourceEncoding','UTF-8')
        self.configuration. set(self.MAIN_SECTION, 'sources', '.')

    def add(self, key, value):
        self.configuration.set(self.MAIN_SECTION, key, value)

    def export(self):
        # Dump the configuration file
        with open(self.output_file, 'wb') as sonar_file:
            self.configuration.write(sonar_file)

## _get_logger_details #########################################################
##
def _get_logger_details(logger):
    details = logger.findCaller()
    return details[0] + ':' + str(details[1])

## _create_logger  ############################################################
##
def _create_logger(logger_level):
    """ Initialise a logger

        Log will be printed in the standard output.
    """
    logging.basicConfig(format='[%(levelname)s] %(message)s', level=logger_level)
    logger = logging.getLogger("qualimetrics")
    return logger


## ProjectTree  ###############################################################
##
class ProjectTree(object):
    def __init__(self, gpr_path, output_dir):
        self.gpr_path = gpr_path
        self.output_file = os.path.join(output_dir, 'project_tree.json')

    def create_tree(self, LOGGER):
        LOGGER.info('=== Generating project json tree file')
         # Set environement, required for script run.sh
        qmt_script = os.path.join(BASE_DIR, 'lib', 'qmt.py')
        os.environ['QMT_PATH'] = qmt_script
        LOGGER.debug(' qmt.py script location: %s' % qmt_script)

        # Execute
        cmd = ['sh', os.path.join(BASE_DIR, 'lib', 'run.sh'),
               '-d', self.gpr_path]
        LOGGER.debug(' Command line used: %s' % ' '.join(cmd))
        p = Popen(cmd, stdout=PIPE, stderr=PIPE, close_fds=True)
        (stdout, stderr) = p.communicate()
        # Check if the output is well a json or an error
        # stop everything if the output is not a well formed json
        try:
            LOGGER.debug(' Checking generated output...')
            json.loads(stdout)
            # Dump the file
            with open(self.output_file, 'w+') as json_file:
                json_file.writelines(stdout);
            return True
        except ValueError as e:
            LOGGER.error(' Unable to generate project json tree')
            LOGGER.error(' %s' % stderr)
            LOGGER.debug(' %s %s' % (_get_logger_details(LOGGER), e.message))

## _parse_command_line ########################################################
##
def _parse_command_line():
    """ Retrieves command line arguments

        This script has one required argument: the project file path (.gpr)
    """

    parser = argparse.ArgumentParser(description='Qualimetrics basic driver')
    parser.add_argument('-P', dest='project_file', help='Project file path.',
                        required=True)
    parser.add_argument('-X', dest='logger_level', action='store_const',
                       default=logging.INFO, const=logging.DEBUG,
                       help='Activate debug logging level. Default is INFO.')
    return parser.parse_args()

## exec_tool ######################################################
##
class ToolExecutor(object):

    def __init__(self, project_tree, report_dir):
        self.project_tree = project_tree
        self.report_dir = report_dir

    def execute(self, tool, script, tool_output, logger):
        logger.info('')
        logger.info('=== Executing %s...' % tool)
        cmd = None
        if script:
            cmd = ['python',
                os.path.join(BASE_DIR, 'lib', script),
                '--json-tree=' + self.project_tree,
                '--report-dir=' + self.report_dir]
            if tool_output:
                cmd.append('--output=' + tool_output)
        else:
            cmd = tool.split(' ')

        logger.debug(' With command line: %s' % ' '.join(cmd))
        try:
            exit_code = call(cmd)
        except OSError as e:
            logger.error('** Skipping tool, command not found: %s **', cmd)
            logger.debug(e)
        return exit_code
## _log_failure ######################################################
##
def _log_failure(LOGGER):
    LOGGER.info('')
    LOGGER.info('-----------------------------------------------')
    LOGGER.info('QUALIMETRICS FAILURE')
    LOGGER.info('-----------------------------------------------')

## Script Entry Point ######################################################
##
def _entry_point():
    """Script entry point"""

    ####################
    #  Initialisation  #
    ####################
    # Tools to execute /!\ to change
    TOOLS = {'gnatmetric' : Tool('gnatmetric', 'gnat metric -x -U -P', 'gnatmetric-report.xml',
                             None, 'sonar.ada.gnatmetric.reportPath'),
             'codepeer'   : Tool('codepeer', 'create_cp_report.py', 'codepeer-report.json',
                            'codepeer.csv.path', 'sonar.ada.codepeer.reportPath'),
             'gcov'       : Tool('gcov', 'create_gcov_report.py', 'gcov-report.json',
                             'gcov.outputDir.path',
                                 'sonar.ada.gcov.reportPath'),
             'gnatcheck'  : Tool('gnatcheck', 'create_gc_report.py',
                                 'gnatcheck-report.xml', 'gnatcheck.out.path',
                                 'sonar.ada.gnatcheck.reportPath')}

    # Retrieve command line arguments
    cmd_line = _parse_command_line()
    # Create a logger
    LOGGER = _create_logger(cmd_line.logger_level)
    # Initialize script configuration
    script_config = ConfigParser.ConfigParser(DEFAULT_CONFIG)
    try:
        script_config.readfp(open(QMT_CONF_FILE))
    except IOError as e:
        LOGGER.error('** qualimetrics.cfg file is required. Cannot be found'
                     ' in current directory **')
        LOGGER.debug(e)
        _log_failure(LOGGER)
        exit(2);
    # Initialize Sonar configuration
    sonar_config = SonarConfiguration()
    # Report output directory, create if doesn't exists
    report_dir = 'reports'
    if not os.path.exists(report_dir):
        os.makedirs(report_dir)
    # Project tree json file
    project_tree = ProjectTree(cmd_line.project_file, report_dir)
    # Object to execute a tool or a script to formatted generate tool report
    tool_executor = ToolExecutor(project_tree.output_file, report_dir)


    ####################
    #    Execution     #
    ####################
    if (project_tree.create_tree(LOGGER)):
        sonar_config.add('sonar.ada.projectTree', project_tree.output_file)
        LOGGER.info('=== Project json tree file generation OK')
        LOGGER.info('')
        for tool in TOOLS:
            LOGGER.debug('%s will process tool: %s' %
                         (_get_logger_details(LOGGER), tool))
            # /!\ Specific processing for GNAT metric - TO BE CHANGED
            if tool == 'gnatmetric':
                output_file = os.path.join(CURRENT_DIR, 'metrix.xml')
                # Retrieve exit code for debug
                exit_code = tool_executor.execute(TOOLS[tool].script_name +
                                          project_tree.gpr_path, None,
                                          None, LOGGER)
                LOGGER.debug('%s Tool has been executed with exit code: %s' %
                             (_get_logger_details(LOGGER), str(exit_code)))
                # Check if the execution succeed
                if exit_code <= 1:
                    # Check metrix.xml has been generated in current directory
                    if os.path.exists(output_file):
                        # Remove old if exists
                        if os.path.exists(os.path.join(report_dir,
                                                       TOOLS[tool].report_name)):
                            os.remove(os.path.join(report_dir, TOOLS[tool].report_name))
                        shutil.move(output_file,
                                    os.path.join(report_dir,
                                                 TOOLS[tool].report_name))
                        sonar_config.add(TOOLS[tool].sonar_key,
                                         os.path.join(report_dir,
                                                      TOOLS[tool].report_name))

                        LOGGER.info('=== GNAT metric OK')
                        LOGGER.info('')
                    else:
                        LOGGER.warning(' Cannot find rxml report from' +
                                       'GNAT metric, in supposed location:' +
                                       ' %s', output_file)
                # If execution failed
                else:
                    LOGGER.warning('=== GNAT metric FAILED')
                    LOGGER.warning('')
            # For all other tools
            else:
                try:
                    exit_code = tool_executor.execute(tool,
                                            TOOLS[tool].script_name,
                                            script_config.get(tool,
                                                                TOOLS[tool].qmt_key),
                                            LOGGER)
                    LOGGER.debug('%s Tool has been executed with exit code: %s' %
                                (_get_logger_details(LOGGER), str(exit_code)))
                    if exit_code <= 1:
                        sonar_config.add(TOOLS[tool].sonar_key,
                                        os.path.join(report_dir,
                                        TOOLS[tool].report_name))
                        LOGGER.info('=== %s OK' % tool)
                        LOGGER.info('')
                    else:
                        LOGGER.warning('=== %s FAILED' % tool)
                        LOGGER.warning('')
                except ConfigParser.NoSectionError as e:
                    LOGGER.warning('** Skipping process of %s\'s output' % tool)
                    LOGGER.warning('** No section in qualimetrics.cfg for' +
                                   ' this tool')
                except ConfigParser.NoOptionError as e_opt:
                    LOGGER.warning('** Skipping process of %s\'s output' % tool)
                    LOGGER.warning('** ' + str(e_opt))

        # Set Sonar configuration
        sonar_config.add('sonar.projectKey', script_config.get('project','project.key'))
        sonar_config.add('sonar.projectVersion',
                         script_config.get('project', 'project.version'))
        sonar_config.add('sonar.projectName', script_config.get('project', 'project.name'))
        sonar_config.export()
        try:
            LOGGER.info('')
            LOGGER.info('=== Running Sonar Runner')
            exit_code = call('sonar-runner')
            LOGGER.debug('sonar-runner executed with exit code: %i', exit_code)
            if exit_code < 1:
                LOGGER.info('=== Sonar Runner OK')
                LOGGER.info('')
                LOGGER.info('-----------------------------------------------')
                LOGGER.info('QUALIMETRICS SUCCESS')
                LOGGER.info('-----------------------------------------------')
            else:
                LOGGER.info('=== Sonar Runner FAIL')
                _log_failure(LOGGER)
        except OSError as e:
            LOGGER.error('*** sonar-runner command not found, cannot feed' +
                         ' sonar with datas')
            LOGGER.debug(e)
            _log_failure(LOGGER)
    else:
        _log_failure(LOGGER)
        exit(3);

## Script Entry Point ######################################################
##
if __name__ == '__main__':
    _entry_point()

