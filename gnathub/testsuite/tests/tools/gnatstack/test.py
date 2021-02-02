"""Check that all files have been created."""

import os

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


TEXT_IO = ["a-textio.ads", "a-textio.ads 509:14"]
S_IMGINT = ["s-imagei.ads", "s-imagei.ads 43:14"]
S_SECSTA = ["s-secsta.ads", "s-secsta.ads 328:9"]
F_ADB = ["f.adb", "f.adb 9:4", "f.adb 21:1"]
F_ADS = ["f.ads"]
SIMPLE_ADB = ["simple.adb", "simple.adb 19:4", "simple.adb 28:4",
              "simple.adb 10:4"]
SIMPLE_4_1 = ["simple.adb 4:1"]
B_SIMPLE_ADB = ["b__simple.adb", "b__simple.adb 210:4"]
SECTIONS = TEXT_IO + S_IMGINT + S_SECSTA + F_ADB + F_ADS + SIMPLE_ADB + SIMPLE_4_1 + B_SIMPLE_ADB

EXTERNAL_CALL = [
    "External Call", "[external call] Put_Line"
]

ENTRY_POINT = [
    "Entry point", ('Main\nTest_Case\nFoo\nImage_Integer')
]

METRICS_SIMPLE_4_1 = [
    "Unknown Global Stack Usage",
    "Static Local Stack Usage"
]


class TestGNATstackSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.project = Project.simple()
        self.project.build()
        self.gnathub = GNAThub(self.project, plugins=['gnatstack'])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = ConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)

        sections = parser.sections()
        for s in SECTIONS:
            self.assertTrue(s in sections, "Missing section: " + s)

        self.assertTrue(parser.has_option(TEXT_IO[1], EXTERNAL_CALL[0]),
                        "Should be an external call")
        self.assertEqual(parser.get(TEXT_IO[1], EXTERNAL_CALL[0]),
                         EXTERNAL_CALL[1],
                         'Wrong message')

        self.assertTrue(parser.has_option(B_SIMPLE_ADB[1], ENTRY_POINT[0]),
                        "Entry point not found")
        self.assertTrue(parser.get(B_SIMPLE_ADB[1],
                                   ENTRY_POINT[0]).endswith(ENTRY_POINT[1]),
                        'unexpected value for the entry point')

        for metric in METRICS_SIMPLE_4_1:
            self.assertTrue(
                parser.has_option(SIMPLE_4_1[0], metric),
                'missing entry for "%s"' % metric)
            # Don't check the values: they are computer dependent
