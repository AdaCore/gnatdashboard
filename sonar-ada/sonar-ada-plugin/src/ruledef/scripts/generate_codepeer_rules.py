"""Generate the contents of the codepeer rules file.

This script takes as input the output of codepeer --list-categories
"""

import sys

# Print the header
print """<?xml version='1.0' encoding='UTF-8'?>
<rules>
"""

# The output of codepeer --list-categories looks like this:

# [ CodePeer Backend message categories ]
# [[ CodePeer Backend check categories ]]
# precondition - a subprogram call that might violate the subprogram's
#  precondition
# postcondition - the subprogram's body may violate its specified
#  postcondition
# user precondition - a call might violate a user specified subprogram's
#  precondition
# (...)
# [ GNAT warnings categories ]
# default GNAT warnings (-gnatwn) - normal warning mode (cancels -gnatws
#  /-gnatwe)
# assertion failure (-gnatw.a) - turn on warnings for failing assertion
# bad fixed value (-gnatwb) - turn on warnings for bad fixed value (not
#  multiple of small)
# (...)
# [ GNATcheck message categories ]
# Abstract_Type_Declarations (GNATCheck) - abstract types
# Annotated_Comments (GNATCheck) - use of comment annotations
# Anonymous_Arrays (GNATCheck) - anonymous array types
# (...)

current_tool_name = ""
current_rule_text = ""
current_rule_label = ""
current_rule_type = ""
current_rule_category = ""


def print_current_rule():
    """Print the current rule"""
    if current_rule_text:
        if current_rule_type:
            print """    <rule>
            <key>{}</key>
            <name>{}</name>
            <description>{}</description>
            <type>{}</type>
            <tag>{}</tag>
            <tag>{}</tag>
          </rule>""".format(current_rule_label.lower(),
                            current_rule_label,
                            current_rule_text,
                            current_rule_type,
                            current_tool_name,
                            current_rule_category)
        else:
            print """    <rule>
            <key>{}</key>
            <name>{}</name>
            <description>{}</description>
            <tag>{}</tag>
            <tag>{}</tag>
          </rule>""".format(current_rule_label.lower(),
                            current_rule_label,
                            current_rule_text,
                            current_tool_name,
                            current_rule_category)


for j in sys.stdin.readlines():
    if j.startswith("[ GNAT warnings"):
        # This is the terminator for Codepeer rules categories
        break

    if j.startswith("[ CodePeer Backend"):
        current_tool_name = "codepeer"
        # Ignore
        continue

    if j.startswith("[[ CodePeer Backend"):
        current_tool_name = "codepeer"

        # Determine rule type and category
        if "check" in j:
            current_rule_category = "check"
            current_rule_type = "CODE_SMELL"

        if "warning" in j:
            current_rule_category = "warning"
            current_rule_type = "CODE_SMELL"

        if "race condition" in j:
            current_rule_category = "race-condition"
            current_rule_type = "BUG"

        if "informational" in j:
            current_rule_category = "informational"
            current_rule_type = None

    else:
        if j.startswith("  "):
            # This is the continuation of the previous rule
            current_rule_text += " " + j.strip()
        else:
            # This is the definition of a rule
            current_rule_label, current_rule_text = j.strip().split(" - ", 1)
            print_current_rule()

# Print the footer
print "</rules>"
