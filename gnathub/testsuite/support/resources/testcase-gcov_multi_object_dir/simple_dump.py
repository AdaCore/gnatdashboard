"""A simple script that uses the GNAThub API to query the SQLite database and
display its content.
"""

import os
import os.path
import sys

sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub


def _fetch():
    """Queries the database to fetch tools, rules and files.

    :return: A dictionnary suitable for being used as kwargs for
        :meth:`_display`.
    :rtype: dict[str,*]

    """

    # Create a dictionary of tools
    tools = {tool.id: tool for tool in GNAThub.Tool.list()}

    # Create a dictionary of rules
    rules = {rule.id: rule for rule in GNAThub.Rule.list()}

    for rule in list(rules.values()):
        rule.tool_name = tools[rule.tool_id].name

    # Traverse all resources looking for files
    files = [r for r in GNAThub.Resource.list() if r.kind == GNAThub.FILE_KIND]
    files.sort(key=lambda x: x.name)

    return {'rules': rules, 'files': files}


def _display(rules, files, stream=sys.stdout):
    """Dumps the content of the database.

    :param dict[int,GNAThub.Rule] rules: List of all rules in the DB.
    :param dict[int,GNAThub.Resource] files: List of all resouces in the DB.
    :param File stream: The output stream in which to send the dump.

    """

    def write(message):
        """Writes the input message in the output stream.

        :param str message: The content to write in the stream.

        """

        print(message, file=stream)

    for resource in files:
        write("[%s]" % os.path.basename(resource.name))

        # list all the messages in this file

        for msg in resource.list_messages():
            if not msg.line:
                write("    [%s:%s] : %s" %
                      (rules[msg.rule_id].tool_name,
                       rules[msg.rule_id].name,
                       msg.data))
            else:
                write("    line %s:\t [%s:%s] : %s" %
                      (msg.line,
                       rules[msg.rule_id].tool_name,
                       rules[msg.rule_id].name,
                       msg.data))


if __name__ == '__main__':
    _display(**_fetch())
