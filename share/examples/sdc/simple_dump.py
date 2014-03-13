import os.path

import GNAThub
from GNAThub import db

# Create a dictionary of tools

tools = {}

for t in GNAThub.Tool.list():
    tools[t.id] = t

# Create a dictionary of rules

rules = {}

for r in GNAThub.Rule.list():
    r.tool_name = tools[r.tool_id].name
    rules[r.id] = r

# Traverse all resources looking for files

files = []

for r in GNAThub.Resource.list():
    if r.kind == db.FILE_KIND:
        files.append(r)

files.sort(key=lambda x: x.name)

for r in files:
    print "[%s]" % os.path.basename(r.name)

    # list all the messages in this file

    for m in r.list_messages():
        if not m.line:
            print "    [%s:%s] : %s" % (
                rules[m.rule_id].tool_name,
                rules[m.rule_id].name, m.data)
        else:
            print "    line %s:\t [%s:%s] : %s" % (
                m.line,
                rules[m.rule_id].tool_name,
                rules[m.rule_id].name, m.data)
