"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

TEXT_IO = ["a-textio.ads 263:14"]
F_ADB = ["f.adb", "f.adb 9:4", "f.adb 21:1"]
F_ADS = ["f.ads"]
SIMPLE_ADB = ["simple.adb", "simple.adb 19:4", "simple.adb 28:4",
              "simple.adb 10:4", "simple.adb 51:5"]
SIMPLE_4_1 = ["simple.adb 4:1"]
B_SIMPLE_ADB = ["b__simple.adb 209:4"]
SECTIONS = TEXT_IO + F_ADB + F_ADS + SIMPLE_ADB + SIMPLE_4_1 + B_SIMPLE_ADB

EXTERNAL_CALL = [
    "External Call", "external call"
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
        self.project.run()
        self.gnathub = GNAThub(self.project, plugins=['gnatstack'])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = SafeConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        sections = parser.sections()
        for s in SECTIONS:
            self.assertTrue(s in sections, "Missing section: " + s)

        self.assertTrue(parser.has_option(TEXT_IO[0], EXTERNAL_CALL[0]),
                        "Should be an external call")
        self.assertEqual(parser.get(TEXT_IO[0], EXTERNAL_CALL[0]),
                         EXTERNAL_CALL[1],
                         'Wrong message')

        self.assertTrue(parser.has_option(B_SIMPLE_ADB[0], ENTRY_POINT[0]),
                        "Entry point not found")
        self.assertTrue(parser.get(B_SIMPLE_ADB[0],
                                   ENTRY_POINT[0]).endswith(ENTRY_POINT[1]),
                        'unexpected value for the entry point')

        for metric in METRICS_SIMPLE_4_1:
            self.assertTrue(
                parser.has_option(SIMPLE_4_1[0], metric),
                'missing entry for "%s"' % metric)
            # Don't check the values: they are computer dependent
