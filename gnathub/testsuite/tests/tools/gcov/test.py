"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script


class TestGcovMultiObjectDirExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Import the project, build it and run it
        self.project = Project.gcov_multi_object_dir()
        self.project.build()
        self.project.run()

        # Run GNAThub with only the gcov plugin
        self.gnathub = GNAThub(self.project, plugins=['gcov'])

        # Extract coverage information from the database
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        self.parser = SafeConfigParser()
        self.parser.optionxform = str

        self.parser.read(script_output_file)

    def getCoverage(self, fn):
        """Returns the lines covered by the tests.

        :param str fn: The filename of the file for which to fetch coverage
            results.
        :returns: list[str]

        """

        return [s for s in self.parser.sections() if s.startswith('%s ' % fn)]

    def testDatabaseContent(self):
        # Test that gcov has been run and that GNAThub found coverage data to
        # collect
        self.assertTrue(not self.parser.has_option('sdc.ads', 'coverage'))

        self.assertEqual(len(self.getCoverage('sdc.adb')), 16)
        self.assertEqual(len(self.getCoverage('values.adb')), 11)
        self.assertEqual(len(self.getCoverage('stack.adb')), 31)
        self.assertEqual(len(self.getCoverage('src2.adb')), 2)
        self.assertEqual(len(self.getCoverage('src3.adb')), 1)
