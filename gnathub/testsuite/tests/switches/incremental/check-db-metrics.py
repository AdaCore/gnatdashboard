"""Check the integrity of the GNAThub results after Codepeer plugin run."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

from support.asserts import (
    assertEmpty, assertEqual, assertFalse, assertListUnorderedEqual, assertTrue
)


# The base directory for PATH comparisons
BASEDIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def contains(tool_name):
    """Check that a tool exists in the database.

    :param tool_name: the name of the tool to check existence for
    :type tool_name: str
    :return: `True` if the tool exists, `False` otherwise
    :rtype: boolean
    """
    for tool in GNAThub.Tool.list():
        if tool.name == tool_name:
            return True
    return False

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

assertTrue(os.path.isfile(GNAThub.database()))
assertEqual(
    relpath(GNAThub.database()),
    os.path.join('obj', 'gnathub', 'gnathub.db')
)

#DB content check

# Check tool exists
TOOL = 'gnatmetric'
assertTrue(contains(TOOL))

# Tools dictionary fron DB
tools = {tool.id: tool for tool in GNAThub.Tool.list()}
assertEqual(len(tools), 1)

# Rules dictionary from DB
rules = {rule.id: rule for rule in GNAThub.Rule.list()}
assertEqual(len(rules), 41)

# Messages from DB
messages = {msg.id: msg for msg in GNAThub.Message.list()}
assertEqual(len(messages), 99)
