"""Generate the contents of the gnatcheck rules file.

This script takes as input the output of gnatcheck -dR -h
"""

import sys
import config

# Print the header
print("""<?xml version='1.0' encoding='UTF-8'?>
<rules>
""")

# The output of gnatcheck -dR -h looks like this:
# gnatcheck currently implements the following rules:
#  Abort_Statements - abort statements - EASY
#  Abstract_Type_Declarations - abstract types - EASY
#  Address_Specifications_For_Initialized_Objects - address specifications for
#   initialized objects - EASY
#  Address_Specifications_For_Local_Objects - address specifications for local
#   objects - EASY
#  Annotated_Comments - use of comment annotations - EASY
#  Anonymous_Arrays - anonymous array types - EASY
#  Anonymous_Subtypes - anonymous subtypes - EASY
#  Binary_Case_Statements - CASE statements that can be replaced with IF
#   statements - EASY
# (...)
# gnatcheck allows activation of the following checks provided by GNAT
# using the same syntax to control these checks as for other rules:
#   Warnings     - compiler warnings - EASY
#   Style_Checks - compiler style checks - TRIVIAL
#   Restrictions - checks made by pragma Restriction_Warnings - EASY

current_rule_text = ""
current_rule_label = ""
extended_checks = False
current_rule_debt = ""

#  Used for technical debt related information
current_rule_debt = ""
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
    return CATEGORY_TO_REMEDIATION_EFFORT.\
        get(td, config.TRIVIAL_REMEDIATION_EFFORT)


def print_current_rule():
    """Print the current rule"""
    if current_rule_text:
        http_link =\
            '\"https://docs.adacore.com/live/wave/asis/html/gnatcheck_rm/'\
            + 'gnatcheck_rm/predefined_rules.html#'\
            + current_rule_label.lower().replace('_', '-') + '\"'
        hyperlink_format = """<a href={}>{}</a>""".\
            format(http_link, current_rule_text)
        body_format = """<body>Click on {} for rule description.</body>""".\
            format(hyperlink_format)
        current_rule_description = """<![CDATA[{}]]>""".format(body_format)

        print("""    <rule>
      <key>{}</key>
      <name>{}</name>
      <description>{}</description>
      <tag>gnatcheck</tag>
      <remediationFunction>{}</remediationFunction>
      <remediationFunctionBaseEffort>{}</remediationFunctionBaseEffort>
    </rule>""".format(current_rule_label.lower(),
                      current_rule_label,
                      current_rule_description,
                      constant_debt,
                      constant_debt_val))


for j in sys.stdin.readlines():
    if j.startswith("gnatcheck allows activation"):
        print_current_rule()
        # Ignore
        continue
    if j.startswith("using the same syntax"):
        extended_checks = True
        # Ignore
        continue
    if j.startswith("gnatcheck"):
        # Ignore
        continue
    if j.startswith("  "):
        if extended_checks:
            # This is the definition of a rule (supposed to take only one line)
            current_rule_label, current_rule_text = j.strip().split(" - ", 1)
            current_rule_label = current_rule_label.strip()

            current_rule_text, current_rule_debt =\
                current_rule_text.strip().split(" - ", 1)
            constant_debt_val = get_effort(current_rule_debt)

            print_current_rule()
        else:
            # This is the continuation of the previous rule
            current_rule_text += " " + j.strip()
            if " - " in current_rule_text:
                current_rule_text, current_rule_debt =\
                    current_rule_text.strip().split(" - ", 1)
                constant_debt_val = get_effort(current_rule_debt)
    else:
        # This is the definition of a rule
        print_current_rule()
        current_rule_label, current_rule_text = j.strip().split(" - ", 1)

        if " - " in current_rule_text:
            current_rule_text, current_rule_debt =\
                current_rule_text.strip().split(" - ", 1)
            constant_debt_val = get_effort(current_rule_debt)


# Print the footer
print("</rules>")
