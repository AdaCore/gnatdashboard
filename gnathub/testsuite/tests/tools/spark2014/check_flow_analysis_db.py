"""Check the integrity of the GNAThub Python module."""

import os
import sys

sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

from support.asserts import (
    assertEqual, assertNotEmpty, assertTrue)


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

# Check that spark2014 tool exists in the DB
TOOL_NAME = 'spark2014'
assertTrue(contains(TOOL_NAME))
tool = GNAThub.Tool(TOOL_NAME)

# Check that rules were created in the Rules table
rules = [rule.id for rule in GNAThub.Rule.list() if rule.tool_id == tool.id]
assertNotEmpty(rules)
assertEqual(len(rules), 12)

# Check that messages were recorded in Messages table
messages = GNAThub.Message.list()
assertNotEmpty(messages)
assertNotEmpty(
    [msg for msg in GNAThub.Message.list() if msg.rule_id in rules])

# Check that all resources were recorded in Ressources table
resources = GNAThub.Resource.list()
assertNotEmpty(resources)
assertEqual(len(resources), 4)
