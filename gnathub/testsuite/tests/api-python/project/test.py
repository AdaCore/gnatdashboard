"""Check that all files have been created."""

from unittest import TestCase
from support import GNAThub, Project


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Run GNAThub with only the sonar-config plugin
        self.gnathub = GNAThub(Project.disabled(), plugins=['sonar-config'])

    def testDatabaseContent(self):
        # Dummy scenario variables
        scenario = {
            'BUILD_MODE': 'Production',
            'VERSION': 'test-0.0.0',
            'BUILD_DIR': '/some/user/workspace/project/build/dir',
            'PROCESSORS': '2'
        }

        # Extract coverage information from the database
        self.gnathub.run(script='check-api.py', scenario_vars=scenario)
