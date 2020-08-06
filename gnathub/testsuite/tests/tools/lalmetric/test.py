"""Check that all files have been created."""

import os
import unittest

from configparser import ConfigParser

from unittest import TestCase
from support import const
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


class TestLALmetricSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        os.environ['USE_LIBADALANG_TOOLS'] = '1'
        self.gnathub = GNAThub(Project.simple(), plugins=['gnatmetric'])

    @unittest.skipIf(const.skipLALToolsTests, 'requires LAL tools')
    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script='check-run.py')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = ConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), FILES)

        for metric, value in EXPECTED_METRICS.items():
            self.assertTrue(
                parser.has_option(SECTION, metric),
                'missing entry for "%s"' % metric)
            self.assertEqual(
                parser.getfloat(SECTION, metric), value,
                'unexpected value for "%s"' % metric)
