"""Check the integrity of the GNAThub Python module."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

from support.asserts import (
    assertEmpty, assertFalse, assertNotEmpty, assertTrue
)


# GNATmetric tool name
TOOL = 'gnatmetric'

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

# Check tool exists
assertTrue(contains(TOOL))
tool = GNAThub.Tool(TOOL)
# Check that rules were created
rules = [rule.id for rule in GNAThub.Rule.list() if rule.tool_id == tool.id]
assertNotEmpty(rules)
# Check that messages were recorded
assertNotEmpty(
    [msg for msg in GNAThub.Message.list() if msg.rule_id in rules])

# Purge the database
GNAThub.Tool.clear_references(TOOL)

# Check that messages were deleted
assertEmpty([msg for msg in GNAThub.Message.list() if msg.rule_id in rules])
# Check that rules were deleted
assertEmpty(
    [rule.id for rule in GNAThub.Rule.list() if rule.tool_id == tool.id])
# Check that tool was deleted
assertFalse(contains(TOOL))
