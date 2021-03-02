from drivers.basic import BasicTestDriver
from e3.testsuite import Testsuite
from e3.testsuite.testcase_finder import ParsedTest
import os
import logging

from support import const


# Directory from where this script is invoked
ORIGIN = os.getcwd()
# Base directory for the testsuite
BASEDIR = os.path.dirname(os.path.realpath(__file__))


class GNAThubTestsuite(Testsuite):
    DRIVERS = {'default': BasicTestDriver}

    def add_options(self, parser):
        parser.add_argument(
            '--with-diff',
            action='store_true',
            default=False,
            help='show diffs on stdout')
        parser.add_argument(
            '--with_sonarqube',
            action='store_true',
            default=False,
            help='SonarQube instance is available')

    @staticmethod
    def find(root, pattern):
        result = []
        for root, dirs, files in os.walk(root):
            root = root.replace('\\', '/')
            for f in files:
                if pattern is None or pattern in f:
                    result.append(root + '/' + f)
        return result

    @staticmethod
    def find_testcases(directory):
        """Find all testcases in the given directory."""

        return set(sorted(
            GNAThubTestsuite.find(directory, pattern='test.py') +
            GNAThubTestsuite.find(directory, pattern='test.sh')))

    def get_test_list(self, sublist):
        if sublist:
            tests = [os.path.relpath(os.path.join(ORIGIN, test), BASEDIR)
                     for test in sublist]
        else:
            basedir = os.path.join(BASEDIR, 'tests')
            tests = [os.path.relpath(os.path.dirname(p), BASEDIR)
                     for p in GNAThubTestsuite.find_testcases(basedir)]
        logging.info('Found %s tests %s', len(tests), tests)
        logging.debug("tests:\n  " + "\n  ".join(tests))
        return [ParsedTest(os.path.basename(test),
                           BasicTestDriver,
                           {},
                           test) for test in tests]

    @property
    def default_driver(self):
        return 'default'
