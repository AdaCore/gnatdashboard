################################################################################
###                                    WHAT IS                               ###
################################################################################

This folder is aimed to contain internal matter content, currently composed of:

    - design/: contains information about the qualimetrics project design
    - test/: contains internal test suite which will not be published with the 
            Sonar plugin and process_testsuite_report.py script that generates
            test results file for GAIA.
    - Makefile: eases the use of the test suite


Details about the makefile:

  The makefile is composed of 4 targets:

    - build: build the Sonar plugin without executing the test suite.
    - install: install the sonar plugin for Sonar. for more details on this
               target see the README located in qualimetrics/sonar-ada-plugin 
               directory.
    - test: execute all test suite concerning the Sonar plugin and the 
            process_testsuite_report.py script.
    - clean: clean qualimetrics/sonar-ada-plugin/ directory from build generated
             folders and generated file form process_testsuite_report.py


################################################################################
###                                     USAGE                                ###
################################################################################

How to generate and retrieve reports for GAIA:

  As said above to generate report for GAIA, juste run make test.
  By default, reports are generated in quaimetrics/internal/test/result/ 
  directory. You can change this location passing the path to the wanted 
  directory to make as below:
    $ make test TEST_RESULTS_DIR="path_to_directory"

