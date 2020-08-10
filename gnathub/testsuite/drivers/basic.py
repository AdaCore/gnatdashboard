from e3.fs import mkdir, sync_tree, cp
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
from e3.env import Env
from drivers import GNAThubTestDriver
import os
import sys
import glob
import logging


class BasicTestDriver(GNAThubTestDriver):
    """ Each test should have:
          - a test.sh or a test.py.

        If the execution returns code 100, it's an XFAIL.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, prev, slot):
        mkdir(self.test_env['working_dir'])
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])
        base = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
        Env().add_search_path('PYTHONPATH', base)

    def check_output(self, out, output):
        def sanitize(output):
            """Remove windows specific characters + exe extension"""
            return output.replace("\r", "").replace(".exe", "")

        if os.path.isfile(out):
            # Found test.out => compare the output with the expected output
            with open(out, "r") as fd:
                expected_output = fd.read()
            if sanitize(output) == expected_output:
                self.result.set_status(TestStatus.PASS)
            else:
                self.result.set_status(TestStatus.ERROR)
        else:
            # No test.out => we don't expect any output from the test
            if output:
                self.result.set_status(TestStatus.ERROR)
            else:
                self.result.set_status(TestStatus.PASS)

    def run(self, prev, slot):
        # Check whether the test should be skipped
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        wd = self.test_env['working_dir']
        compare_output = False
        if os.path.isfile(os.path.join(wd, 'test.sh')):
            cmd_line = ["sh", "test.sh"]
            # For the sh test we only care about the output (ignore the error
            # code => most script will fail and we are monitoring the output
            # error)
            compare_output = True
        else:
            cmd_line = [sys.executable,
                        "-m", "unittest", "discover", "-p", "test*.py"]
        process = Run(
            cmd_line,
            cwd=wd,
            timeout=120,
            ignore_environ=False)
        output = process.out

        if output:
            # If there's an output, capture it
            self.result.out = output

        if process.status:
            # Nonzero status?
            if process.status == 100:
                # This one is an xfail
                self.result.set_status(TestStatus.XFAIL)
            elif compare_output:
                self.check_output(os.path.join(wd, 'test.out'), output)
            else:
                # Unknown status
                self.result.set_status(TestStatus.ERROR)
        else:
            self.result.set_status(TestStatus.PASS)

        self.push_result()
