"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


FILES = sorted(['simple.adb', 'f.adb', 'f.ads'])
SECTION = 'simple.adb'

EXPECTED_METRICS = {
    'all_lines': 51,
    'code_lines': 28,
    'comment_lines': 12,
    'eol_comments': 0,
    'comment_percentage': 30.00,
    'blank_lines': 11,
    'statement_complexity': 1.75,
    'expression_complexity': 0.00,
    'essential_complexity': 1.75,
    'cyclomatic_complexity': 1.75,
    'max_loop_nesting': 2.00
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
        self.assertListEqual(sorted(parser.sections()), FILES)

        for metric, value in EXPECTED_METRICS.iteritems():
            self.assertTrue(
                parser.has_option(SECTION, metric),
                'missing entry for "%s"' % metric)
            self.assertEqual(
                parser.getfloat(SECTION, metric), value,
                'unexpected value for "%s"' % metric)
