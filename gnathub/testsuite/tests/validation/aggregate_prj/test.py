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
    ('obj', 'gnatcheck_1.out'),
    ('obj', 'gnatcheck_2.out'),
    ('obj', 'gnatcheck-source-list_1.out'),
    ('obj', 'gnatcheck-source-list_2.out'),
    ('obj', 'gnathub', 'gnathub.db'),
    ('obj', 'gnathub', 'logs', 'gnatcheck.log'),
    ('obj', 'gnathub', 'sonar', 'sonar-project.properties')
]


class TestAggregatePrjExample(TestCase):
    def setUp(self):
        plugins = ['gnatcheck', 'sonar-config']
        self.gnathub = GNAThub(Project.aggregate_prj(), plugins=plugins)

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
