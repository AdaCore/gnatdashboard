import os.path

import GNAThub
from GNAThub import db

# Create a dictionary of rules

rules = {}

for r in GNAThub.Rule.list():
    rules[r.id] = r.name

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
            print "    %s = %s" % (rules[m.rule_id], m.data)
        else:
            print "    line %s:\t%s = %s" % (
                m.line, rules[m.rule_id], m.data)
