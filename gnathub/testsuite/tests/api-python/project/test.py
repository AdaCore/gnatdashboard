"""Check that all files have been created."""

from unittest import TestCase
from support import GNAThub, Project


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Run GNAThub with only the sonar-config plugin
        self.gnathub = GNAThub(Project.disabled(), plugins=['sonar-config'])

    def testDatabaseContent(self):
        # Extract coverage information from the database
        self.gnathub.run(script='check-api.py')
