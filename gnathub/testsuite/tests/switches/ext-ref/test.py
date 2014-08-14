"""Check that all files have been created."""

from unittest import TestCase
from support import GNAThub, Project


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Run GNAThub with only the sonar-config plugin
        self.gnathub = GNAThub(Project.disabled(), plugins=['sonar-config'])

    def testExternalRefsSwitch(self):
        # Pass a -Xkey=value option to GNAThub
        self.gnathub.run(external_refs={'key': 'value'})
