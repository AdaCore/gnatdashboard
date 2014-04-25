import collections
import os
import sys

import GNAThub

from ConfigParser import SafeConfigParser


def collect_data(writer):
    """Collects all data stored in the database using GNAThub's ORM routines.

    Stores all resources and their associated metric in a ConfigParser object.

    :param ConfigParser.SafeConfigParser writer: Container for the database
        metrics and values.

    """

    # Create a dictionary of rules
    rules = {rule.id: rule.name for rule in GNAThub.Rule.list()}
    files = [r for r in GNAThub.Resource.list() if r.kind == GNAThub.FILE_KIND]

    for resource in sorted(files, key=lambda x: x.name):
        basename = os.path.basename(resource.name)
        messages = resource.list_messages()

        writer.add_section(basename)
        for message in [message for message in messages if not message.line]:
            writer.set(basename, rules[message.rule_id], message.data)

        line_messages = collections.defaultdict(lambda: [])
        for message in [message for message in messages if message.line]:
            line_messages[message.line].append(message)

        for line, messages in line_messages.iteritems():
            section = '%s %d' % (basename, line)
            writer.add_section(section)

            for message in messages:
                writer.set(section, rules[message.rule_id], message.data)


if __name__ == '__main__':
    # Create a case sensitive config writer
    writer = SafeConfigParser()
    writer.optionxform = str

    # Collect the data from the database and display them
    collect_data(writer)
    writer.write(sys.stdout)
