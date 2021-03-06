"""Check that all files have been created."""

from unittest import TestCase
from support.mock import GNAThub, Project


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Run GNAThub with only the sonar-config plugin
        self.gnathub = GNAThub(Project.simple(), plugins=['sonar-config'])

    def testDatabaseContent(self):
        # Test the Python plug-in API
        self.gnathub.run(script='check-api.py')
