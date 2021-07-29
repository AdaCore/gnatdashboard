"""Check that LALmetric executed correctly."""

import sys
import os
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

from support.asserts import assertIn, assertNotEmpty

assertIn('lalmetric', [tool.name for tool in GNAThub.Tool.list()])
lalmetric = GNAThub.Tool('lalmetric')
lalmetric_rules = [
    rule.id for rule in GNAThub.Rule.list() if rule.tool_id == lalmetric.id
]
assertNotEmpty(lalmetric_rules)
assertNotEmpty(
    [msg for msg in GNAThub.Message.list() if msg.rule_id in lalmetric_rules]
)
