"""Check the integrity of the GNAThub Python module."""

import os
import sys

import GNAThub

from support.asserts import (
    assertEmpty, assertEqual, assertIsNotNone, assertNotEmpty, assertRaises
)


base = GNAThub.Project.source_file('simple.adb')
resource = GNAThub.Resource.get(base)
assertIsNotNone(resource)
tool = GNAThub.Tool('test-tool')
assertIsNotNone(tool)
rule = GNAThub.Rule('test-rule', 'test-rule-name', GNAThub.RULE_KIND, tool)
assertIsNotNone(rule)
prop0 = GNAThub.Property('test-prop-0', 'test-prop-name-0')
assertIsNotNone(prop0)
prop1 = GNAThub.Property('test-prop-1', 'test-prop-name-1')
assertIsNotNone(prop1)
msg0 = GNAThub.Message(rule, 'test message', properties=None)
assertIsNotNone(msg0)
msg1 = GNAThub.Message(rule, 'test message', properties=[prop0, prop1])
assertIsNotNone(msg1)
for msg in msg0, msg1:
    resource.add_message(msg)
assertEmpty(msg0.get_properties())
assertNotEmpty(msg1.get_properties())
msg1_prop0 = msg1.get_properties()[0]
assertEqual(prop0.name, msg1_prop0.name)
assertEqual(prop0.identifier, msg1_prop0.identifier)

with assertRaises(Exception):
    GNAThub.Message(rule, 'test message', properties=0)

with assertRaises(Exception):
    GNAThub.Message(rule, 'test message', properties=prop0)
