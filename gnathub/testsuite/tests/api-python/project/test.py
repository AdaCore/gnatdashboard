"""Check that all files have been created."""

import os

from unittest import TestCase
from support import GNAThub, Project, Script


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Run GNAThub with only the sonar-config plugin
        self.gnathub = GNAThub(Project.disabled(), plugins=['sonarconfig'])

    def testDatabaseContent(self):
        # Extract coverage information from the database
        self.gnathub.run(script='check-api.py')
