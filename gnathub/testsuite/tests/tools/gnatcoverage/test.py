"""Check that all files have been created."""

from collections import namedtuple
import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


class TestCoverageExhaustiveExample(TestCase):

    Violation = namedtuple('Violation', 'level column message')
    CoverageRecord = namedtuple('CoverageRecord',
                                'file rule line column message')

    EXPECTED_COVERAGE = {
        'do_nothing.adb':  {
            1: Violation('coverage', 0,  'NO_CODE'),
            2: Violation('coverage', 0,  'NO_CODE'),
            3: Violation('coverage', 0,  'COVERED'),
            4: Violation('coverage', 0,  'NO_CODE')
        },
        'main.adb':  {
            1: Violation('coverage', 0,  'NO_CODE'),
            2: Violation('coverage', 0,  'NO_CODE'),
            3: Violation('coverage', 0,  'NO_CODE'),
            4: Violation('coverage', 0,  'NO_CODE'),
            5: Violation('coverage', 0,  'NO_CODE'),
            6: Violation('coverage', 0,  'NO_CODE'),
            7: Violation('coverage', 0,  'COVERED'),
            8: Violation('coverage', 0,  'COVERED'),
            9: Violation('coverage', 0,  'COVERED'),
            10: Violation('coverage', 0,  'COVERED'),
            11: Violation('coverage', 0,  'NO_CODE')
        },
        'test_stmt.adb':  {
            1: Violation('coverage', 0,  'NO_CODE'),
            2: Violation('coverage', 0,  'NO_CODE'),
            3: Violation('coverage', 0,  'NO_CODE'),
            4: Violation('coverage', 0,  'NO_CODE'),
            5: Violation('coverage', 0,  'NO_CODE'),
            6: Violation('coverage', 0,  'COVERED'),
            7: Violation('coverage', 0,  'NO_CODE'),
            8: Violation('coverage', 0,  'COVERED'),
            9: Violation('coverage', 0,  'NO_CODE'),
            10: [
                Violation('statement', 7,  'statement not executed'),
                Violation('coverage', 0,  'NOT_COVERED')
            ],
            11: Violation('coverage', 0,  'NO_CODE'),
            12: Violation('coverage', 0,  'NO_CODE'),
            13: Violation('coverage', 0,  'NO_CODE'),
            14: [
                Violation('statement', 4, 'statement not executed'),
                Violation('coverage', 0,  'PARTIALLY_COVERED')
            ],
            15: Violation('coverage', 0,  'NO_CODE'),
            16: Violation('coverage', 0,  'NO_CODE'),
            17: Violation('coverage', 0,  'COVERED'),
            18: [
                Violation('statement', 7,  'statement not executed'),
                Violation('coverage', 0,  'NOT_COVERED')
            ],
            19: Violation('coverage', 0,  'NO_CODE'),
            20: Violation('coverage', 0,  'NO_CODE')
        },
        'test_decision.adb': {
            1: Violation('coverage', 0,  'NO_CODE'),
            2: Violation('coverage', 0,  'NO_CODE'),
            3: Violation('coverage', 0,  'NO_CODE'),
            4: Violation('coverage', 0,  'NO_CODE'),
            5: Violation('coverage', 0,  'NO_CODE'),
            6: [
                Violation('decision', 4,  'decision outcome FALSE never exercised'),
                Violation('coverage', 0,  'PARTIALLY_COVERED')
            ],
            7: Violation('coverage', 0,  'COVERED'),
            8: Violation('coverage', 0,  'NO_CODE'),
            9: Violation('coverage', 0,  'NO_CODE'),
            10: Violation('coverage', 0,  'NO_CODE'),
            11: [
                Violation('decision', 4, 'decision outcome TRUE never exercised'),
                Violation('coverage', 0,  'PARTIALLY_COVERED'),
            ],
            12: [
                Violation('statement', 7,  'statement not executed'),
                Violation('coverage', 0,  'NOT_COVERED')
            ],
            13: Violation('coverage', 0,  'NO_CODE'),
            14: Violation('coverage', 0,  'NO_CODE'),
            15: [
                Violation('statement', 7,  'statement not executed'),
                Violation('coverage', 0,  'NOT_COVERED')
            ],
            16: [
                Violation('statement', 10, 'statement not executed'),
                Violation('coverage', 0,  'NOT_COVERED')
            ],
            17: Violation('coverage', 0,  'NO_CODE'),
            18: Violation('coverage', 0,  'NO_CODE'),
            19: Violation('coverage', 0,  'NO_CODE'),
            20: Violation('coverage', 0,  'COVERED'),
            21: Violation('coverage', 0,  'NO_CODE'),
            22: Violation('coverage', 0,  'COVERED'),
            23: Violation('coverage', 0,  'COVERED'),
            24: Violation('coverage', 0,  'NO_CODE'),
            25: Violation('coverage', 0,  'NO_CODE'),
            26: Violation('coverage', 0,  'NO_CODE')
        },
        'test_mcdc.adb': {
            1: Violation('coverage', 0,  'NO_CODE'),
            2: Violation('coverage', 0,  'NO_CODE'),
            3: Violation('coverage', 0,  'NO_CODE'),
            4: Violation('coverage', 0,  'NO_CODE'),
            5: Violation('coverage', 0,  'NO_CODE'),
            6: [
                Violation(
                    'condition', 4,
                    'condition has no independent influence pair, MC/DC not achieved'
                ),
                Violation('coverage', 0,  'PARTIALLY_COVERED')
            ],
            7: Violation('coverage', 0,  'COVERED'),
            8: Violation('coverage', 0,  'NO_CODE'),
            9: Violation('coverage', 0,  'NO_CODE')
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
                            content.level,
                            content.column,
                            content.message
                        ))
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
