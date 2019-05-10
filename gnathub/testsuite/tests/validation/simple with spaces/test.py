"""Check that all files have been created."""

import os

from unittest import TestCase
from support.mock import GNAThub, Project


DIRS = [
    ('obj', 'gnathub'),
    ('obj', 'gnathub', 'logs'),
    ('obj', 'gnathub', 'sonar'),
]

FILES = [
    ('obj', 'gnatcheck.out'),
    ('obj', 'gnatcheck-source-list.out'),
    ('obj', 'gnathub', 'gnathub.db'),
    ('obj', 'gnathub', 'logs', 'gnatcheck.log'),
    ('obj', 'gnathub', 'logs', 'gnatmetric.log'),
    ('obj', 'gnathub', 'sonar', 'sonar-project.properties'),
    ('obj', 'simple.adb.metrix'),
    ('obj', 'metrix.xml')
]


class TestSimpleWithSpacesExample(TestCase):
    def setUp(self):
        plugins = ['gnatmetric', 'gnatcheck', 'sonar-config']
        self.gnathub = GNAThub(Project.simple_with_spaces(), plugins=plugins)

    def testFilesCreated(self):
        # Check that all expected files exist
        for fqn in FILES:
            path = reduce(os.path.join, fqn, self.gnathub.project.install_dir)
            self.assertTrue(os.path.isfile(path), 'missing file: %s' % path)

    def testDirsCreated(self):
        # Check that all expected directories exist
        for fqn in DIRS:
            path = reduce(os.path.join, fqn, self.gnathub.project.install_dir)
            self.assertTrue(os.path.isdir(path), 'missing dir: %s' % path)
