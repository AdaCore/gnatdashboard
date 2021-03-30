"""Generate the contents of the gnatstack rules file.

This script takes as input the output of
         gnatstack --list-categories
"""

import sys

# Print the header
print("""<?xml version='1.0' encoding='UTF-8'?>
<rules>
""")

# The output of gnatstack --list-categoried looks like this:
# gnatstack currently implements the following rules:

#  Unknown Global Stack Usage - unknown part of the total stack usage
#  Static Global Stack Usage - total stack usage
#  Unknown Local Stack Usage - unknown part of the local stack usage
#  Static Local Stack Usage - local stack usage
#  Indirect Call - function pointers
#  External Call - subprograms for which no stack information is available.
#  Potential Cycle - potential recursive function call
#  Unbounded Frame - frames of functions that  use dynamically sized local objects
#  Entry point - program entry point from which to start the analysis
#  Accuracy - stack computation accuracy

current_rule_text = ""
current_rule_label = ""
extended_checks = False


def print_current_rule():
    """Print the current rule"""
    if current_rule_text:
        print("""    <rule>
      <key>{}</key>
      <name>{}</name>
      <description>{}</description>
      <tag>gnatstack</tag>
    </rule>""".format(current_rule_label,
                      current_rule_label,
                      current_rule_text))


for j in sys.stdin.readlines():
    # This is the definition of a rule
    current_rule_label, current_rule_text = j.strip().split(" - ", 1)
    print_current_rule()

# Print the footer
print("</rules>")
