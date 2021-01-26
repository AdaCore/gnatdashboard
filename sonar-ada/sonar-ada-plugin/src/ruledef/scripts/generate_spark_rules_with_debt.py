"""Generate the contents of the codepeer rules file.

This script takes as input the output of codepeer --list-categories
"""

import sys
import config

# Print the header
print("""<?xml version='1.0' encoding='UTF-8'?>
<rules>
""")

# The output of gnatprove --list-categories looks like this:

# [Flow analysis check categories]
# ALIASING - aliasing between subprogram parameters - Aliasing between
#  formal parameters or global objects. - EASY
# CALL_IN_TYPE_INVARIANT - invalid call in type invariant - A type invariant
#  calls a boundary subprogram for the type. - EASY
# (...)
# [Proof check categories]
# VC_DIVISION_CHECK(run-time-check) - divide by zero - Check that the
#  second operand of the division, mod or rem operation is different from
#  zero. - MEDIUM
# VC_INDEX_CHECK(run-time-check) - index check - Check that the given index
#  is within the bounds of the array. - MEDIUM
# VC_OVERFLOW_CHECK(run-time-check) - overflow check - Check that the result
#  of the given integer arithmetic operation is within the bounds of the
#  base type. - MEDIUM
# (...)
# [Proof warnings categories]
# VC_INCONSISTENT_PRE - precondition always False - Warn if precondition is
#  found to be always False - EASY
# VC_INCONSISTENT_POST - postcondition always False - Warn if postcondition is
#  found to be always False - EASY
# VC_INCONSISTENT_ASSUME - pragma Assume always False - Warn if pragma Assume
#  is found to be always False - EASY
# (...)

tool_name = ""
rule_text = ""
rule_name = ""
rule_label = ""
rule_type = ""
rule_category = ""

rule_debt = ""
constant_debt = "CONSTANT_ISSUE"
constant_debt_val = "0min"

# SonarQube coding rules remediation effort categories
CATEGORY_TO_REMEDIATION_EFFORT = {
    'trivial': config.TRIVIAL_REMEDIATION_EFFORT,
    'easy': config.EASY_REMEDIATION_EFFORT,
    'medium': config.MEDIUM_REMEDIATION_EFFORT,
    'major': config.MAJOR_REMEDIATION_EFFORT,
    'high': config.HIGH_REMEDIATION_EFFORT,
    'complex': config.COMPLEX_REMEDIATION_EFFORT
}


def get_effort(category):
    """Get corresponding remediation effort for a given category

    :param str category: rule category for technical debt computation
    """

    td = category.lower().replace(' ', '')
    return CATEGORY_TO_REMEDIATION_EFFORT. \
        get(td, config.TRIVIAL_REMEDIATION_EFFORT)


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
            <remediationFunction>{}</remediationFunction>
            <remediationFunctionBaseEffort>{}</remediationFunctionBaseEffort>
    </rule>""".format(rule_name.lower(),
                      rule_label,
                      rule_text,
                      rule_type,
                      tool_name,
                      rule_category,
                      constant_debt,
                      constant_debt_val))


for j in sys.stdin.readlines():
    tool_name = "spark2014"

    if j.startswith("[Flow analysis"):
        # Set rule category and ignore
        if "error" in j:
            rule_category = "flow-analysis-error"
            rule_type = "BUG"

        if "check" in j:
            rule_category = "flow-analysis-check"
            rule_type = "BUG"

        if "warning" in j:
            rule_category = "flow-analysis-warning"
            rule_type = "CODE_SMELL"
        continue

    if j.startswith("[Proof"):
        # Set rule category and ignore
        if "check" in j:
            rule_category = "proof-check"
            rule_type = "BUG"

        if "warning" in j:
            rule_category = "proof-warning"
            rule_type = "CODE_SMELL"
        continue

    else:
        if j.startswith("  "):
            # This is the continuation of the previous rule
            # (normally never here since rules are listed as one line by rule)
            rule_text += " " + j.strip()
        else:
            # This is the definition of a rule
            rule_name, rule_label, rule_text, rule_debt =\
                j.strip().split(" - ", 3)

            # Handle specific rules categories when specified
            # (like, VC_ASSERT(assertion) - assertion - Check that ....)
            if "(" in rule_name:
                rule_name, rule_category = rule_name.split("(")
                # rule_name = crt_rule_name
                rule_category = rule_category.replace(")", "")

            # Get the associated effort
            constant_debt_val = get_effort(rule_debt)
            print_current_rule()

# Print the footer
print("</rules>")
