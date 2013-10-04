"""
This module contains support functions for all test.py
"""

import os
import sys
from gnatpython.ex import Run

#  Change directory
TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)


def echo(arg):
    """Invoke echo

    PARAMETERS
      arg: string to echo on stdout
    """

    print Run(['echo', arg]).out
