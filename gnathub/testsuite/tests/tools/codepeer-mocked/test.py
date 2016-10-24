"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import (
    CodePeerExecutable, CodePeerMsgReaderExecutable, GNAThub, Project, Script
)


RESULTS = sorted([
    'f.adb',
    'f.adb 18:32',
    'f.ads',
    'simple.adb',
    'simple.adb 30:24',
    'simple.adb 37:8'
])


class TestCodePeerSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        self.gnathub = GNAThub(
            Project.simple(), plugins=['codepeer'],
            mocks=[CodePeerExecutable, CodePeerMsgReaderExecutable])

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = SafeConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertListEqual(sorted(parser.sections()), RESULTS)
