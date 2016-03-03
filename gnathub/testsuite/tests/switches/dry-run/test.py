"""Check that dry run mode has been honored."""

import os.path

from unittest import TestCase
from support.mock import GNAThub, Project


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

    def testDryRunModeNoSideEffect(self):
        PROJECT = Project.disabled()
        PLUGINS = ['sonar-config']
        DB = os.path.join(PROJECT.install_dir, 'obj', 'gnathub', 'gnathub.db')

        # Execute GNAThub with option --dry-run
        # Run GNAThub with only the sonar-config plugin
        gnathub = GNAThub(PROJECT, plugins=PLUGINS, dry_run=True)
        assert not os.path.exists(DB), 'Database should not have been created'

        # Execute GNAThub without option --dry-run
        gnathub.run(plugins=PLUGINS)
        assert os.path.exists(DB), 'Database should have been created'
