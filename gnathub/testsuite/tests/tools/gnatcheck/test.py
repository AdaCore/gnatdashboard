"""Check that all messages have been reported."""

import os

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

# Expected result sections
F_ADB = 'f.adb 10:7'

P_ADB = 'p.adb 3:4'

P_ADS = 'p.ads 6:4'

PG_ADB_1 = 'p_g.adb 40:10'
PG_ADB_2 = 'p_g.adb 49:10'
PG_ADB_3 = 'p_g.adb 20:4'
PG_ADB_4 = 'p_g.adb 20:13'
PG_ADB_5 = 'p_g.adb 14:13'

PG_ADS_1 = 'p_g.ads 5:4'
PG_ADS_2 = 'p_g.ads 11:4'
PG_ADS_3 = 'p_g.ads 12:4'
PG_ADS_4 = 'p_g.ads 18:13'

PGG_ADS_1 = 'p_g_g.ads 7:4'
PGG_ADS_2 = 'p_g_g.ads 7:17'

SIMPLE_ADB_1 = 'simple.adb 34:17'
SIMPLE_ADB_2 = 'simple.adb 3:6'
SIMPLE_ADB_3 = 'simple.adb 44:4'

SECTIONS = sorted([
    'simple.adb',
    'f.adb', 'f.ads',
    'p.adb', 'p.ads',
    'p_g.adb', 'p_g.ads',
    'p_g_g.ads',
    F_ADB,
    P_ADB,
    P_ADS,
    PG_ADB_1, PG_ADB_2, PG_ADB_3, PG_ADB_4, PG_ADB_5,
    PG_ADS_1, PG_ADS_2, PG_ADS_3, PG_ADS_4,
    PGG_ADS_1, PGG_ADS_2,
    SIMPLE_ADB_1, SIMPLE_ADB_2, SIMPLE_ADB_3
])


class TestGNATcheckSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.gnathub = GNAThub(Project.simple_gnatcheck(), plugins=['gnatcheck'])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = ConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), SECTIONS)

        self.assertTrue(
            parser.has_option(F_ADB, 'warnings'),
            'missing "warnings" entry')
        self.assertTrue(
            parser.has_option(F_ADB, 'identifier_casing'),
            'missing "identifier_casing" entry')

        self.assertTrue(
            parser.has_option(P_ADB, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(P_ADS, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')

        self.assertTrue(
            parser.has_option(PG_ADB_1, 'improper_returns'),
            'missing "improper_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADB_2, 'improper_returns'),
            'missing "improper_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADB_3, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADB_4, 'warnings'),
            'missing "warnings" entry')
        self.assertTrue(
            parser.has_option(PG_ADB_5, 'warnings'),
            'missing "warnings" entry')

        self.assertTrue(
            parser.has_option(PG_ADS_1, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADS_2, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADS_3, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(PG_ADS_4, 'recursive_subprograms'),
            'missing "recursive_subprograms" entry')

        self.assertTrue(
            parser.has_option(PGG_ADS_1, 'unconstrained_array_returns'),
            'missing "unconstrained_array_returns" entry')
        self.assertTrue(
            parser.has_option(PGG_ADS_2, 'warnings'),
            'missing "warnings" entry')

        self.assertTrue(
            parser.has_option(SIMPLE_ADB_1, 'improper_returns'),
            'missing "improper_returns" entry')
        self.assertTrue(
            parser.has_option(SIMPLE_ADB_3, 'identifier_suffixes'),
            'missing "identifier_suffixes" entry')
        self.assertTrue(
            parser.has_option(SIMPLE_ADB_3, 'identifier_prefixes'),
            'missing "identifier_prefixes" entry')
