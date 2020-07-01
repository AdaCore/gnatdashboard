"""Check that all files have been created."""

import os

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


TEXT_IO = ["a-textio.ads", "a-textio.ads 508:14"]
S_IMGINT = ["s-imgint.ads", "s-imgint.ads 39:14"]
S_SECSTA = ["s-secsta.ads", "s-secsta.ads 315:9"]
F_ADB = ["f.adb", "f.adb 9:4", "f.adb 21:1"]
F_ADS = ["f.ads"]
SIMPLE_ADB = ["simple.adb", "simple.adb 19:4", "simple.adb 28:4",
              "simple.adb 10:4", "simple.adb 51:5"]
SIMPLE_4_1 = ["simple.adb 4:1"]
B_SIMPLE_ADB = ["b__simple.adb", "b__simple.adb 179:27",
                "b__simple.adb 162:24",
                "b__simple.adb 8:1", "b__simple.adb 66:17",
                "b__simple.adb 45:20", "b__simple.adb 161:21",
                "b__simple.adb 34:4", "b__simple.adb 163:29",
                "b__simple.adb 165:21", "b__simple.adb 167:24",
                "b__simple.adb 169:18", "b__simple.adb 171:23",
                "b__simple.adb 173:33", "b__simple.adb 175:19",
                "b__simple.adb 177:24", "b__simple.adb 51:20",
                "b__simple.adb 181:25", "b__simple.adb 183:20",
                "b__simple.adb 185:35", "b__simple.adb 58:20",
                "b__simple.adb 188:32", "b__simple.adb 191:15",
                "b__simple.adb 192:15", "b__simple.adb 65:4",
                "b__simple.adb 194:18", "b__simple.adb 196:32",
                "b__simple.adb 69:17", "b__simple.adb 198:31",
                "b__simple.adb 200:23", "b__simple.adb 202:21",
                "b__simple.adb 204:18", "b__simple.adb 205:18",
                "b__simple.adb 84:4", "b__simple.adb 213:4",
                "b__simple.adb 219:17", "b__simple.adb 222:17",
                "b__simple.adb 41:4", "b__simple.adb 120:17",
                "b__simple.adb 255:1"]
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
        self.project.run()
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

        self.assertTrue(parser.has_option(B_SIMPLE_ADB[34], ENTRY_POINT[0]),
                        "Entry point not found")
        self.assertTrue(parser.get(B_SIMPLE_ADB[34],
                                   ENTRY_POINT[0]).endswith(ENTRY_POINT[1]),
                        'unexpected value for the entry point')

        for metric in METRICS_SIMPLE_4_1:
            self.assertTrue(
                parser.has_option(SIMPLE_4_1[0], metric),
                'missing entry for "%s"' % metric)
            # Don't check the values: they are computer dependent
