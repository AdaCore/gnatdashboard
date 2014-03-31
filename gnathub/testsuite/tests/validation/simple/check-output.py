"""Check that all files have been created."""

# pylint: disable=C0103
# Disable "Invalid module name" (not a module, but a script)

import os
import sys


EXPECTED_DIRS = [
    ('obj', 'gnathub'),
    ('obj', 'gnathub', 'logs'),
    ('obj', 'gnathub', 'sonar'),
]

EXPECTED_FILES = [
    ('obj', 'empty'),
    ('obj', 'gnatcheck.out'),
    ('obj', 'gnatcheck-source-list.out'),
    ('obj', 'gnathub', 'gnathub.db'),
    ('obj', 'gnathub', 'logs', 'gnatcheck.log'),
    ('obj', 'gnathub', 'logs', 'gnatmetric.log'),
    ('obj', 'gnathub', 'sonar', 'sonar-project.properties'),
    ('obj', 'hello.adb.metrix'),
    ('obj', 'metrix.xml')
]


def check_files():
    """
    Check that all expected files and directories exists.

    RAISES
        AssertionError on failure
    """

    root = os.path.dirname(os.path.realpath(__file__))

    for fragments in EXPECTED_DIRS:
        path = reduce(os.path.join, fragments, root)
        if not os.path.isdir(path):
            print >> sys.stderr, '%s: no such directory' % path

    for fragments in EXPECTED_FILES:
        path = reduce(os.path.join, fragments, root)
        if not os.path.isfile(path):
            print >> sys.stderr, '%s: no such file' % path

if __name__ == '__main__':
    check_files()
