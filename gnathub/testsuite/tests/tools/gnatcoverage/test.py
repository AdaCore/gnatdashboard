"""Check that all files have been created."""

from collections import namedtuple
import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support import GNAThub, Project, Script


class TestCoverageExhaustiveExample(TestCase):

    OK = '1'
    Violation = namedtuple('Violation', 'level column message')
    CoverageRecord = namedtuple('CoverageRecord',
                                'file rule line column message')

    EXPECTED_COVERAGE = {
        'main.adb':       {7: OK, 8: OK, 9: OK, 10: OK},
        'do_nothing.adb': {3: OK},
        'test_stmt.adb':  {
            6:  OK,
            8:  OK,
            10: Violation('stmt', 7,  'not executed'),
            14: Violation('stmt', 42, 'not executed'),
            17: OK,
            18: [
                Violation('stmt', 7,  'not executed'),
                Violation('stmt', 19, 'not executed')
            ],
        },
        'test_decision.adb': {
            6:  Violation('decision', 7,  'outcome FALSE never exercised'),
            7:  OK,
            11: Violation('decision', 11, 'outcome TRUE never exercised'),
            12: Violation('stmt', 7,  'not executed'),
            15: Violation('stmt', 7,  'not executed'),
            16: Violation('stmt', 10, 'not executed'),
            20: OK,
            22: OK,
            23: OK,
        },
        'test_mcdc.adb': {
            6:  Violation(
                    'mcdc', 7,
                    'has no independent influence pair, MC/DC not achieved'
                ),
            7:  OK,
        },
    }

    def setUp(self):
        self.longMessage = True

        # Just import the project. Do no try to build it nor to run it: we
        # don't expect GNATcoverage to be available during testsuite runs, so
        # use the precomputed coverage reports instead.
        self.project = Project.coverage_exhaustive()

        # Run GNAThub with only the GNATcoverage plugin
        self.gnathub = GNAThub(self.project, plugins=['gnatcoverage'])

        # Extract coverage information from the database
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        self.parser = SafeConfigParser()
        self.parser.optionxform = str

        self.parser.read(script_output_file)

    def structured_to_list_coverage(self, struct):
        """
        Turn structured coverage expectations into a list of CoverageRecord.

        See TestCoverag.ExhaustiveExample.EXPECTED_COVERAGE for a structured
        coverage example.
        """
        result = []
        for filename, record in struct.iteritems():
            for line, record in record.iteritems():
                # Shortcut: if there is only one message on a line, do not
                # require a sequence of messages.
                if isinstance(record, (basestring, self.Violation)):
                    record = [record]

                for content in record:
                    def get_rec(rule, column, message):
                        return self.CoverageRecord(
                            filename, rule, line, column, message)
                    if isinstance(content, self.Violation):
                        result.append(get_rec(
                            'coverage', content.column, '0'))
                        result.append(get_rec(
                            'gnatcov_{}'.format(content.level),
                            content.column,
                            content.message
                        ))
                    else:
                        result.append(get_rec('coverage', 1, content))

        result.sort()
        return result

    def config_to_list_coverage(self, config):
        """Turn a ConfigParser instance into a list of CoverageRecord."""
        result = []

        for section in config.sections():
            # There are no file-wide sections we are interested in: skip them
            try:
                filename, sloc = section.rsplit(None, 1)
                line, column = sloc.split(':')
            except ValueError:
                continue
            line = int(line)
            column = int(column)
            for name, value in config.items(section):
                result.append(self.CoverageRecord(
                    filename, name, line, column, value
                ))

        result.sort()
        return result

    def testDatabaseContent(self):
        # Test that GNAThub extract the coverage information we expect it to
        self.assertEqual(
            self.config_to_list_coverage(self.parser),
            self.structured_to_list_coverage(self.EXPECTED_COVERAGE)
        )
