import glob
import os
import os.path
import sys
import logging
import time
import xml.dom.minidom


#######################
# process_test_result #
#######################

############
# CONSTANT #
############

BASEDIR = '../..'
SUREFIRE_REPORT_DIR = 'sonar-ada-plugin/target/surefire-reports/'
RESULTS_DIR = 'internal/test/result/'

def process_test_result():
    """ Process the test results and produce the res files.
    """

    os.chdir(BASEDIR)
    tests = {}

    def explore(n, current_test=""):
        """ Explore mininode n.
            tests is the tests dictionary,
            current_test is the current test.
        """
        if n.localName == 'testcase':
            current_test = n.getAttribute("name")
            tests[current_test] = {}
            if current_test == 'testz999':
                tests[current_test]['status'] = 'XFAIL'
            else:
                tests[current_test]['status'] = 'OK'

        elif n.localName is None:
             if current_test in tests:
                if not "diff" in tests[current_test]:
                    tests[current_test]["diff"] = ''
                tests[current_test]["diff"] = (
                    tests[current_test]["diff"] + n.data)

        elif n.localName == 'error':
            if current_test != '':
                if not "diff" in tests[current_test]:
                    tests[current_test]["diff"] = ''

                tests[current_test]['status'] = 'CRASH'
                tests[current_test]["diff"] = (
                  tests[current_test]["diff"] + n.getAttribute("message")
                                               + "\n")

        elif n.localName == 'failure':
             if current_test != '':
                if not "diff" in tests[current_test]:
                    tests[current_test]["diff"] = ''

                tests[current_test]['status'] = 'DIFF'
                tests[current_test]["diff"] = (
                  tests[current_test]["diff"] + n.getAttribute("message")
                                               + "\n")

        elif n.localName not in ['testsuite', 'testsuites', 'properties',
                                 'property', 'system-out', 'system-err']:
            # The list above is the nodes where we know we have no
            # processing to do
            logging.info("Error: unknown xml node: %s" % n.localName)

        for a in n.childNodes:
            explore(a, current_test)

    path = os.path.join(SUREFIRE_REPORT_DIR, "*xml")
    print path
    for file in glob.glob(path):
        explore(xml.dom.minidom.parse(file))

    results_file = open(os.path.join(RESULTS_DIR, "results"), "w")

    for a in tests:
        status = tests[a]['status']
        results_file.write("%s:%s:\n" % (a, status))
        t = open(os.path.join(RESULTS_DIR, a + ".result"), "w")
        t.write("%s:\n" % status)
        t.close()

        if status != "OK" and status != 'XFAIL':
            out = open(os.path.join(RESULTS_DIR, a + ".diff"), "w")
            test_failure_result = tests[a]['diff'] + "\n"
            out.write(test_failure_result.encode("utf-8"))
            out.close()

    results_file.close()

def __main__():
    process_test_result()

if __name__ == '__main__':
    __main__()
