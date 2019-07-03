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
    'all lines': 51,
    'code lines': 28,
    'comment lines': 12,
    'end-of-line comments': 0,
    'comment percentage': 30.00,
    'blank lines': 11,
    'statement complexity': 1.75,
    'expression complexity': 0.00,
    'cyclomatic complexity': 1.75,
    'essential complexity': 1.75,
    'maximum loop nesting': 2.00
}

EXPECTED_EMETRICS = {
    'all lines': 48,
    'code lines': 26,
    'comment lines': 12,
    'end-of-line comments': 0,
    'comment percentage': 31.57,
    'blank lines': 10,
    'public subprograms': 1,
    'all subprogram bodies': 4,
    'all statements': 10,
    'all declarations': 9,
    'logical SLOC': 19,
    'maximal unit nesting': 1,
    'maximal construct nesting': 5,
    'all parameters': 0,
    'statement complexity': 1,
    'expression complexity': 0,
    'cyclomatic complexity': 1,
    'essential complexity': 1,
    'maximum loop nesting': 0,
    'extra exit points': 0
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
                'missing entry for "%s"' % emetric)
            self.assertEqual(
                parser.getfloat(ESECTION, emetric), value,
                'unexpected value for "%s"' % emetric)
