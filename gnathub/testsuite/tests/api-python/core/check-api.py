"""Check the integrity of the GNAThub Python module."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

import platform

from support.asserts import (
    assertEmpty, assertEqual, assertFalse, assertListUnorderedEqual, assertTrue
)


# The base directory for PATH comparisons
BASEDIR = os.path.dirname(os.path.realpath(sys.argv[0]))


def relpath(path):
    """Returns the relative path to :param:`path` from BASEDIR.

    :param str path: The full path.
    :returns: str

    """

    return os.path.relpath(path, BASEDIR)


assertTrue(os.path.isdir(GNAThub.root()))
assertEqual(relpath(GNAThub.root()), os.path.join('obj', 'gnathub'))

assertTrue(os.path.isdir(GNAThub.logs()))
assertEqual(relpath(GNAThub.logs()), os.path.join('obj', 'gnathub', 'logs'))

assertTrue(GNAThub.quiet())
assertFalse(GNAThub.verbose())
assertFalse(GNAThub.dry_run())
assertTrue(GNAThub.runners_only())
assertFalse(GNAThub.reporters_only())

assertTrue(os.path.isfile(GNAThub.database()))
assertEqual(
    relpath(GNAThub.database()),
    os.path.join('obj', 'gnathub', 'gnathub.db')
)

# Default for jobs number is 0
assertEqual(GNAThub.jobs(), 0)

# The plugin list is expected to be empty
assertEqual(len(GNAThub.plugins()), 0)

# We ensure that the core and extra plugins directories exist
repos = GNAThub.repositories()
for kind in ('system', 'global'):
    assertTrue(os.path.isdir(repos[kind]))

# GNAThub.run
TO_BE_ECHOED = 'this is the message to display on the standard output'

process = GNAThub.Run('echo', ('echo', TO_BE_ECHOED))

assertEqual(process.wait(), 0)
assertEqual(process.status, 0)
assertEqual(process.name, 'echo')
assertEqual(process.cmdline_image(), "echo '%s'" % TO_BE_ECHOED)
assertEqual(process.output(), os.path.join(GNAThub.logs(), 'echo.log'))

assertTrue(os.path.isfile(process.output()))

with open(process.output(), 'r') as logs:
    content = logs.read().strip()
    if platform.system() == 'Windows':
        TO_BE_ECHOED = '"' + TO_BE_ECHOED + '"'
    assertEqual(content, TO_BE_ECHOED)

assertListUnorderedEqual(
    GNAThub.tool_args('codepeer'),
    ['-msg-output-only', '-j0', 'positional-arg'])
assertListUnorderedEqual(
    GNAThub.tool_args('codepeer_msg_reader'), ['-msg-output-only'])
assertEmpty(GNAThub.tool_args('unknown-tool'))
