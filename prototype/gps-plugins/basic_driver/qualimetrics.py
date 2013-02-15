#! /usr/bin/env python

## Imports #################################################################
##
import os
import ConfigParser
import argparse
import os.path
from subprocess import call, Popen, PIPE
import shlex
import shutil
import sys

##################
#    CONSTANTS   #
##################
BASE_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
LOGS_DIR = 'logs'
SONAR_FILE_NAME = 'sonar-project.properties'
# Default Qualimetrics configuration
DEFAULT_CONFIG = {'project.version'            : '1.0',
                  'project.key'                : 'default',
                  'project.name'               : 'default',
                  'gnatmetric.switches'        : '-x -U',
                  'qualimetrics.reportDir.path': 'reports',
                  'gcov.outputDir.path'        : None}
# Sonar configuration for an Ada project
SONAR_CONF={'sonar.projectKey'                : None,
            'sonar.projectName'               : None,
            'sonar.projectVersion'            : None,
            'sonar.language'                  : 'ada',
            'sonar.sourceEncoding'            : 'UTF-8',
            'sources'                         : '.',
            'sonar.ada.src.projectTree'           : None,
            'sonar.ada.gnatmetric.reportPath' : None,
            'sonar.ada.codepeer.reportPath'   : None,
            'sonar.ada.gcov.reportPath'       : None}


## log #################################################################
##
def  log(tool, output):
    """log output in logs/tool.log"""

    if not os.path.exists(LOGS_DIR):
        os.makedirs(LOGS_DIR)
    log_file = tool.replace(' ', '') + '.log'
    with open(os.path.join(LOGS_DIR, log_file), 'w+') as log:
        log.writelines(output)


## exec_tool #################################################################
##
def exec_tool(tool, reportDir, switches):

    print '====== Running ' + tool
    print '  switches: ' , switches
    # Execute
    cmd = tool
    if switches:
        cmd = cmd  + ' ' + switches
    cmd = filter(None, cmd.split(' '))
    try:
        p = Popen(cmd, stdout=PIPE, stderr=PIPE, close_fds=True)
        output = p.stdout.read()
        print output

        # Move report to qualimetrics report directory, specific to GNATmetric
        # just for prototype version
        if tool == 'gnat metric' and os.path.exists('metrix.xml'):
            shutil.move('metrix.xml', os.path.join(reportDir,
                                                'gnatmetric-report.xml'))
    except OSError as e:
        print 'Unable to execute tool: ' + tool
        print e

## _parse_command_line #################################################################
##
def _parse_command_line():
    """ Retrieves command line arguments """

    parser = argparse.ArgumentParser(description='Qualimetrics basic driver')
    parser.add_argument('configuration_file', help='Configuration file')
    return parser.parse_args()


## _entry_point #############################################################
##
def _entry_point():
    """Script entry point"""

    # Initialisation
    cmd_line = _parse_command_line()
    config = ConfigParser.ConfigParser(DEFAULT_CONFIG)

    try:
        #######################################
        # Retrieve Qualimetrics configuration #
        #######################################
        config.readfp(open(cmd_line.configuration_file))

        # Set sonar conf
        SONAR_CONF['sonar.projectKey'] = config.get('Project', 'project.key')
        SONAR_CONF['sonar.projectName'] = config.get('Project', 'project.name')
        SONAR_CONF['sonar.projectVersion'] = config.get('Project', 'project.version')

        # Create report directory if doesn't exist
        reportDir = config.get('Qualimetrics',
                               'qualimetrics.reportDir.path')
        if not os.path.exists(reportDir):
            os.makedirs(reportDir)

        # Set project file related information
        gpr_path = config.get('Project', 'project.gpr.path')
        project_tree_path = os.path.join(os.getcwd(), 'project_tree.json')

        #################################
        # Generate Qualimetrics reports #
        #################################

        #---- Project json tree file path ----
        print '==== Generating project json tree file'
        print '  project: ' + gpr_path
        # Set environement, required for script run.sh, /!\ to be changed
        qmt = os.path.join(BASE_DIR, 'lib', 'qmt.py')
        os.environ['QMT_PATH'] = qmt
        # Execute
        cmd = ['sh', os.path.join(BASE_DIR, 'lib', 'run.sh'), '-d', gpr_path]
        p = Popen(cmd, stdout=PIPE, stderr=PIPE)
        output = p.stdout.read()
        # Dump in a file in reportDir
        with open(project_tree_path, 'w+') as json_file:
            json_file.writelines(output);
        # Set sonar conf
        SONAR_CONF['sonar.ada.src.projectTree'] = 'project_tree.json'

        #---- GNATmetric execution ----
        # Retrieve user conf for GNATmetric execution
        gc_switches = config.get('Gnatmetric', 'gnatmetric.switches')
        # Execute
        exec_tool('gnat metric', reportDir, gc_switches + ' -P' + gpr_path)
        # Set sonar conf /!\to be changed
        SONAR_CONF['sonar.ada.gnatmetric.reportPath'] = os.path.join(reportDir, 'gnatmetric-report.xml')

        #---- Generates gcov report ----
        # Retrieve user conf for gcov, i.e gcov output directory if not object
        # directory and set sitches for scirpt that generates gcov report
        gcov_out = config.get('GCov', 'gcov.outputDir.path')
        script = 'python' + ' ' + os.path.join(BASE_DIR,
                                               'lib', 'create_gcov_report.py')
        gcov_switches = '--json-tree=' + project_tree_path + ' --report-dir=' + reportDir
        if gcov_out:
            gcov_switches = switches + ' --gcov-path=' + gcov_out
        # Execute
        exec_tool(script, reportDir, gcov_switches)
        # Set sonar conf
        SONAR_CONF['sonar.ada.gcov.reportPath'] = os.path.join(reportDir, 'gcov-report.json')

        #---- Generates codepeer reports ----
        # Retrieve user conf and set switches for script
        cp_csv = config.get('Codepeer', 'codepeer.csv.path')
        script = 'python' + ' ' + os.path.join(BASE_DIR,
                                               'lib', 'create_cp_report.py')
        cp_switches = '--json-tree=' + project_tree_path + ' --output=' + cp_csv + ' --report-dir=' + reportDir
        # Execute
        exec_tool(script, reportDir, cp_switches)
        # Set sonar conf
        SONAR_CONF['sonar.ada.codepeer.reportPath'] = os.path.join(reportDir, 'codepeer-report.json')

        #---- Generates sonar properties file ----
        print '====== Generating sonar properties file\n'
        sonar_config =  ConfigParser.ConfigParser()
        sonar_config.add_section('Sonar')
        # Enable case sensitive for key name
        sonar_config.optionxform = str
        for key in SONAR_CONF:
            if SONAR_CONF[key]:
                sonar_config.set('Sonar', key, SONAR_CONF[key])
        # Dump the file
        with open('sonar-project.properties', 'wb') as sonar_file:
            sonar_config.write(sonar_file)

        ########################
        # Execute sonar runner #
        ########################
        exec_tool('sonar-runner', None, None)

        print '---------------------------------------------------------'
        print '    Process terminated - You can now browse Sonar :)'
        print '---------------------------------------------------------\n'

    # Cannot open configuration file
    except IOError as e:
        print "Cannot open file: " + cmd_line.configuration_file
        print e
    except ConfigParser.NoOptionError as opt:
        print '***  /!\ Stopping process  ***'
        print '  ', opt
        print '  Mandatory property in configuration file is missing'

## Script Entry Point ######################################################
##
if __name__ == '__main__':
    _entry_point()

