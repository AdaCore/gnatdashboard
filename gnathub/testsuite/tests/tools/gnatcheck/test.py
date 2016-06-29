"""Check that all messages have been reported."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


F_ADB_10_7 = 'f.adb 10:7'
SIMPLE_ADB_33_17 = 'simple.adb 33:17'
SIMPLE_ADB_43_4 = 'simple.adb 43:4'
SECTIONS = sorted([
    'simple.adb', 'f.adb', 'f.ads', F_ADB_10_7, SIMPLE_ADB_33_17,
    SIMPLE_ADB_43_4
])


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
            parser.has_option(F_ADB_10_7, 'warnings'),
            'missing "warnings" entry')
        self.assertTrue(
            parser.has_option(SIMPLE_ADB_33_17, 'improper_returns'),
            'missing "improper_returns" entry')
        self.assertTrue(
            parser.has_option(F_ADB_10_7, 'identifier_casing'),
            'missing "identifier_casing" entry')
        self.assertTrue(
            parser.has_option(SIMPLE_ADB_43_4, 'identifier_suffixes'),
            'missing "identifier_suffixes" entry')
        self.assertTrue(
            parser.has_option(SIMPLE_ADB_43_4, 'identifier_prefixes'),
            'missing "identifier_prefixes" entry')
