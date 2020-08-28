"""Check that all files have been created."""

import os

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

ELEMENTS = sorted(['project1.adb',
                   'project1.adb 3:1',
                   'project2.adb',
                   'project2.adb 3:1'])

SECTION = 'project1.adb'
ESECTION = 'project1.adb 3:1'

EXPECTED_METRICS = {
    'all lines': 6,
    'code lines': 5,
    'comment lines': 0,
    'end-of-line comments': 0,
    'comment percentage': 0.00,
    'blank lines': 1,
    'Average lines in body': 4.00,
    'spark lines': 0,
    'statement complexity': 1.00,
    'expression complexity': 0.00,
    'cyclomatic complexity': 1.00,
    'essential complexity': 1.00,
    'maximum loop nesting': 0.00
}

EXPECTED_EMETRICS = {
    'all lines': 4,
    'code lines': 4,
    'comment lines': 0,
    'end-of-line comments': 0,
    'comment percentage': 0.00,
    'blank lines': 0,
    'public subprograms': 1,
    'all subprogram bodies': 1,
    'all statements': 1,
    'all declarations': 1,
    'logical SLOC': 2,
    'maximal construct nesting': 1,
    'all parameters': 0,
    'statement complexity': 1,
    'expression complexity': 0,
    'cyclomatic complexity': 1,
    'essential complexity': 1,
    'maximum loop nesting': 0,
    'extra exit points': 0
}


class TestGNATmetricAbstractWithMultiObjDir(TestCase):
    def setUp(self):
        self.longMessage = True
        self.project = Project.abstract_with_multi_obj_dir()
        self.gnathub = GNAThub(self.project, plugins=['gnatmetric'],
            tool_args={'gnatmetric': ['-U']})

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = ConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), ELEMENTS)

        for metric, value in EXPECTED_METRICS.items():
            self.assertTrue(
                parser.has_option(SECTION, metric),
                'missing entry for "%s"' % metric)
            self.assertEqual(
                parser.getfloat(SECTION, metric), value,
                'unexpected value for "%s"' % metric)

        for emetric, value in EXPECTED_EMETRICS.items():
            self.assertTrue(
                parser.has_option(ESECTION, emetric),
                'missing entry for "%s"' % emetric)
            self.assertEqual(
                parser.getfloat(ESECTION, emetric), value,
                'unexpected value for "%s"' % emetric)
