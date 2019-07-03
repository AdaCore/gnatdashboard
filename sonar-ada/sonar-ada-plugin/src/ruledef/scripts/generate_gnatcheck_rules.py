"""Generate the contents of the gnatcheck rules file.

This script takes as input the output of gnatcheck -h
"""

import sys

# Print the header
print """<?xml version='1.0' encoding='UTF-8'?>
<rules>
"""

# The output of gnatcheck -h looks like this:
# gnatcheck currently implements the following rules:
#  Abstract_Type_Declarations - abstract types
#  Annotated_Comments - use of comment annotations
#  Anonymous_Arrays - anonymous array types
#  Anonymous_Subtypes - anonymous subtypes
#  Binary_Case_Statements - CASE statements that can be replaced with IF
#   statements
#  (...)
#  gnatcheck allows activation of the following checks provided by GNAT
#  using the same syntax to control these checks as for other rules:
#   Warnings     - compiler warnings
#   Style_Checks - compiler style checks
#   Restrictions - checks made by pragma Restriction_Warnings

current_rule_text = ""
current_rule_label = ""
extended_checks = False


def print_current_rule():
    """Print the current rule"""
    if current_rule_text:
        print """    <rule>
      <key>{}</key>
      <name>{}</name>
      <description>{}</description>
      <tag>gnatcheck</tag>
    </rule>""".format(current_rule_label.lower(),
                      current_rule_label,
                      current_rule_text)


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
            # This is the definition of a rule
            current_rule_label, current_rule_text = j.strip().split(" - ", 1)
            current_rule_label = current_rule_label.strip()
            print_current_rule()
        else:
            # This is the continuation of the previous rule
            current_rule_text += " " + j.strip()
    else:
        # This is the definition of a rule
        print_current_rule()
        current_rule_label, current_rule_text = j.strip().split(" - ", 1)


# Print the footer
print "</rules>"
