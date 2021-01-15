"""Generate the contents of the spark rules file.

This script takes as input the output of gnatprove --list-categories
"""

import sys

# Print the header
print("""<?xml version='1.0' encoding='UTF-8'?>
<rules>
""")

# The output of gnatprove --list-categories looks like this:

# [Flow analysis check categories]
# ALIASING - aliasing between subprogram parameters - Aliasing between
#  formal parameters or global objects.
# CALL_IN_TYPE_INVARIANT - invalid call in type invariant - A type
#  invariant calls a boundary subprogram for the type.
# (...)
# [Proof check categories]
# VC_DIVISION_CHECK - divide by zero - Check that the second operand of
#  the division, mod or rem operation is different from zero.
# VC_INDEX_CHECK - index check - Check that the given index is within the
#  bounds of the array.
# VC_OVERFLOW_CHECK - overflow check - Check that the result of the given
#  integer arithmetic operation is within the bounds of the base type.
# (...)
# [Proof warnings categories]
# VC_INCONSISTENT_PRE - precondition always False - Warn if precondition is
#  found to be always False
# VC_INCONSISTENT_POST - postcondition always False - Warn if postcondition
#  is found to be always False
# VC_INCONSISTENT_ASSUME - pragma Assume always False - Warn if pragma Assume
#  is found to be always False
# (...)

tool_name = ""
rule_text = ""
rule_name = ""
rule_label = ""
rule_type = ""
rule_category = ""


def print_current_rule():
    """Print the current rule"""
    if rule_text:
        print("""    <rule>
            <key>{}</key>
            <name>{}</name>
            <description>{}</description>
            <type>{}</type>
            <tag>{}</tag>
            <tag>{}</tag>
    </rule>""".format(rule_name.lower(),
                      rule_label,
                      rule_text,
                      rule_type,
                      tool_name,
                      rule_category))


for j in sys.stdin.readlines():
    tool_name = "spark2014"

    if j.startswith("[Flow analysis"):
        # Set rule category and ignore
        rule_category = "flow-analysis-check"
        rule_type = "BUG"
        continue

    if j.startswith("[Proof"):
        # Set rule category and ignore
        if "check" in j:
            rule_category = "run-time-check"
            rule_type = "BUG"

        if "warning" in j:
            rule_category = "warning"
            rule_type = "CODE_SMELL"
        continue

    else:
        if j.startswith("  "):
            # This is the continuation of the previous rule
            # (normally never here since rules are listed as one line by rule)
            rule_text += " " + j.strip()
        else:
            # This is the definition of a rule
            rule_name, rule_label, rule_text = j.strip().split(" - ", 2)
            print_current_rule()

# Print the footer
print("</rules>")
