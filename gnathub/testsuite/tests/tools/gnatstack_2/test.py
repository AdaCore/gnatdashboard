"""
Check gnatstack entry point when it's containing a frame without location.
(<_gnat_rcheck_CE_Range_Check> is refering an entity which doesn't exist in
the project and gnatstack can't link it to a file thus it doesn't have a
location)
"""

import os

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

MAIN = ["main.adb 1:1"]

MATRIX = ["matrix_math.adb 3:4"]

B_MAIN = ["b__main.adb 168:4"]

UNBOUNDED_FRAME = [
    "Unbounded Frame", '[unbounded frame] "*"'
]

ENTRY_POINT = [
    "Entry point", ('Main\nMain\n"*"\n<_gnat_rcheck_CE_Range_Check>')
]

METRICS_MATRIX = [
    "Unknown Global Stack Usage",
    "Static Local Stack Usage"
]


class TestGNATstackSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.project = Project.simple_gnatstack()
        self.project.build()
        self.gnathub = GNAThub(self.project, plugins=['gnatstack'])

        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        self.parser = ConfigParser()
        self.parser.optionxform = str

        self.parser.read(script_output_file)

    def testDatabaseContent(self):
        self.assertTrue(self.parser.has_option(MATRIX[0], UNBOUNDED_FRAME[0]),
                        "Should be an unbounded frame")

        self.assertTrue(self.parser.has_option(B_MAIN[0], ENTRY_POINT[0]),
                        "Entry point not found")

        for metric in METRICS_MATRIX:
            self.assertTrue(
                self.parser.has_option(MATRIX[0], metric),
                'missing entry for "%s"' % metric)
            # Don't check the values: they are computer dependent
