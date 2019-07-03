"""Generate the contents of the codepeer rules file.

This script takes as input the output of codepeer --list-categories
"""

import sys

import config

# Print the header
print """<?xml version='1.0' encoding='UTF-8'?>
<rules>
"""

# The output of codepeer --list-categories-with-debts looks like this:

# [ CodePeer Backend message categories ]
# [[ CodePeer Backend check categories ]]
# precondition - a subprogram call that might violate the subprogram's
#  precondition - EASY
# postcondition - the subprogram's body may violate its specified
#  postcondition - EASY
# user precondition - a call might violate a user specified subprogram's
#  precondition - EASY
# validity check - possible read of uninitialized or invalid value (CWE 457)
#  - EASY
# access check - dereference of a possibly null reference (CWE 476) - EASY
# (...)

# [ GNAT warnings categories ]
# default GNAT warnings (-gnatwn) - normal warning mode (cancels -gnatws/
# -gnatwe) - EASY
# assertion failure (-gnatw.a) - turn on warnings for failing assertion - EASY
# bad fixed value (-gnatwb) - turn on warnings for bad fixed value (not
#  multiple of small) - EASY
# biased representation (-gnatw.b) - turn on warnings for biased
#  representation - EASY
# constant conditional (-gnatwc) - turn on warnings for constant conditional
#  - EASY
# (...)
# [ GNATcheck message categories ]
# Abort_Statements (GNATCheck) - abort statements - EASY
# Abstract_Type_Declarations (GNATCheck) - abstract types - EASY
# Address_Specifications_For_Initialized_Objects (GNATCheck) - address
#  specifications for
# initialized objects (GNATCheck) - EASY
# Address_Specifications_For_Local_Objects (GNATCheck) - address
#  specifications for local objects (GNATCheck) - EASY
# (...)

current_tool_name = ""
current_rule_text = ""
current_rule_label = ""
current_rule_type = ""
current_rule_category = ""

current_rule_debt = ""
constant_debt = "CONSTANT_ISSUE"
constant_debt_val = "0min"

lal_checkers = False

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
    if current_rule_text:
        if current_rule_type:
            print """    <rule>
            <key>{}</key>
            <name>{}</name>
            <description>{}</description>
            <type>{}</type>
            <tag>{}</tag>
            <tag>{}</tag>
            <remediationFunction>{}</remediationFunction>
            <remediationFunctionBaseEffort>{}</remediationFunctionBaseEffort>
          </rule>""".format(current_rule_label.lower(),
                            current_rule_label,
                            current_rule_text,
                            current_rule_type,
                            current_tool_name,
                            current_rule_category,
                            constant_debt,
                            constant_debt_val)
        else:
            print """    <rule>
            <key>{}</key>
            <name>{}</name>
            <description>{}</description>
            <tag>{}</tag>
            <tag>{}</tag>
            <remediationFunction>{}</remediationFunction>
            <remediationFunctionBaseEffort>{}</remediationFunctionBaseEffort>
          </rule>""".format(current_rule_label.lower(),
                            current_rule_label,
                            current_rule_text,
                            current_tool_name,
                            current_rule_category,
                            constant_debt,
                            constant_debt_val)


for j in sys.stdin.readlines():
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

    elif j.startswith("[ GNAT warnings"):
        current_tool_name = "codepeer"
        current_rule_category = "warning"
        current_rule_type = "CODE_SMELL"
        continue

    elif j.startswith("[ GNATcheck "):
        current_tool_name = "codepeer"
        current_rule_category = "warning"
        current_rule_type = "CODE_SMELL"
        continue

    elif j.startswith("[ LAL-checkers"):
        lal_checkers = True
        current_tool_name = "codepeer"
        current_rule_category = "warning"
        current_rule_type = "CODE_SMELL"
        continue

    else:
        if j.startswith("  "):
            # This is the continuation of the previous rule
            # (To be never here since rules are listed as one line by rule)
            current_rule_text += " " + j.strip()
        elif lal_checkers:
            # LAL checkers: keep only significant information
            current_rule_label, current_rule_text, dummy, current_rule_debt =\
                j.strip().split(" - ", 3)
            constant_debt_val = get_effort(current_rule_debt)
            print_current_rule()
        else:
            # This is the definition of a rule
            current_rule_label, current_rule_text, current_rule_debt =\
                j.strip().split(" - ", 2)
            constant_debt_val = get_effort(current_rule_debt)
            print_current_rule()


# Print the footer
print "</rules>"
