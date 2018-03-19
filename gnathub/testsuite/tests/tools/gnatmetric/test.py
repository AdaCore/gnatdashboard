"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

ELEMENTS = sorted(['f.adb',
                   'f.adb 3:1',
                   'f.adb 9:4',
                   'f.ads',
                   'f.ads 1:1',
                   'f.ads 3:4',
                   'simple.adb',
                   'simple.adb 10:4',
                   'simple.adb 19:4',
                   'simple.adb 28:4',
                   'simple.adb 4:1'])

SECTION = 'simple.adb'
ESECTION = 'simple.adb 4:1'

EXPECTED_METRICS = {
    'all_lines': 51,
    'code_lines': 28,
    'comment_lines': 12,
    'eol_comments': 0,
    'comment_percentage': 30.00,
    'blank_lines': 11,
    'statement_complexity': 1.75,
    'expression_complexity': 0.00,
    'cyclomatic_complexity': 1.75,
    'essential_complexity': 1.75,
    'max_loop_nesting': 2.00
}

EXPECTED_EMETRICS = {
    'all_lines': 48,
    'code_lines': 26,
    'comment_lines': 12,
    'eol_comments': 0,
    'comment_percentage': 31.57,
    'blank_lines': 10,
    'public_subprograms': 1,
    'all_subprograms': 4,
    'all_stmts': 10,
    'all_dcls': 9,
    'lsloc': 19,
    'unit_nesting': 1,
    'construct_nesting': 5,
    'all_parameters': 0,
    'statement_complexity': 1,
    'expression_complexity': 0,
    'cyclomatic_complexity': 1,
    'essential_complexity': 1,
    'max_loop_nesting': 0,
    'extra_exit_points': 0
}

class TestGNATmetricSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.gnathub = GNAThub(Project.simple(), plugins=['gnatmetric'])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)
        
        parser = SafeConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), ELEMENTS)

        for metric, value in EXPECTED_METRICS.iteritems():
            self.assertTrue(
                parser.has_option(SECTION, metric),
                'missing entry for "%s"' % metric)
            self.assertEqual(
                parser.getfloat(SECTION, metric), value,
                'unexpected value for "%s"' % metric)

        for emetric, value in EXPECTED_EMETRICS.iteritems():
            self.assertTrue(
                parser.has_option(ESECTION, emetric),
                'missing entry for "%s"' % metric)
            self.assertEqual(
                parser.getfloat(ESECTION, emetric), value,
                'unexpected value for "%s"' % emetric)
