"""Check that all messages have been reported."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


COMPILER_WARNING = 'f.adb 10:7'
IMPROPER_RETURNS = 'simple.adb 33:17'
SECTIONS = sorted(['simple.adb', 'f.adb', 'f.ads',
                   COMPILER_WARNING, IMPROPER_RETURNS])


class TestGNATcheckSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.gnathub = GNAThub(Project.simple(), plugins=['gnatcheck'])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = SafeConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), SECTIONS)

        self.assertTrue(
            parser.has_option(COMPILER_WARNING, 'warnings'),
            'missing "warnings" entry')
        self.assertTrue(
            parser.has_option(IMPROPER_RETURNS, 'improper_returns'),
            'missing "improper_returns" entry')
