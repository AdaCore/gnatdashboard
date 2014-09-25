"""Check that all files have been created."""

import os

from ConfigParser import SafeConfigParser

from unittest import TestCase
from support import GNAThub, Project, Script


class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

        # Import the project, build it and run it
        self.project = Project.simple()
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
        self.assertTrue(not self.parser.has_option('f.ads', 'coverage'))

        self.assertEqual(len(self.getCoverage('simple.adb')), 18)
        self.assertEqual(len(self.getCoverage('f.adb')), 5)
        self.assertEqual(len(self.getCoverage('f.ads')), 0)
