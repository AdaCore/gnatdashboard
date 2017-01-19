# GNAThub (GNATdashboard)
# Copyright (C) 2016-2017, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

"""GNAThub reporters.

It massages the collected results of the various input tools in a common format
prior to export.
"""

import GNAThub

import collections
import logging
import os
import time

from enum import Enum

import pygments.lexers
import pygments.util

from pygments import highlight
from pygments.formatters import HtmlFormatter


def count_extra_newlines(lines):
    """Count the number of leading and trailing newlines.

    :rtype: int, int
    """
    leading_newline_count, trailing_newline_count = 0, 0
    for line in lines:
        if not line:
            leading_newline_count += 1
        else:
            break
    for line in reversed(lines):
        if not line:
            trailing_newline_count += 1
        else:
            break
    return leading_newline_count, trailing_newline_count


class CoverageStatus(Enum):
    """Coverage status enumeration."""

    NO_CODE, COVERED, NOT_COVERED, PARTIALLY_COVERED = range(4)


class MessageRanking(Enum):
    """Ranking values for messages collected and reported by GNAThub."""

    INFO, MINOR, MAJOR, CRITICAL, BLOCKER = range(5)


class _HtmlFormatter(HtmlFormatter):
    """Custom implementation of the Pygments' HTML formatter."""

    def wrap(self, source, _):
        # The default wrap() implementation adds a <div> and a <pre> tag.
        return source


class ReportBuilder(object):
    """Report builder."""

    def __init__(self):
        rules = GNAThub.Rule.list()
        tools = GNAThub.Tool.list()
        self.log = logging.getLogger(__name__)
        self._rules_by_id = {rule.id: rule for rule in GNAThub.Rule.list()}
        self._tools_by_id = {tool.id: tool for tool in tools}
        self._tools_by_name = {tool.name.lower(): tool for tool in tools}

    def _generate_source_tree(self, transform=None):
        """Generate the initial project structure.

        {
            "project_name_1": {
                "source_dir_path_1": [
                    "source_filename_1", "source_filename_2"
                ],
                ...
            },
            ...
        }

        This structure will then be populated with relevant metrics.

        :param transform: optional routine that takes 3 parameters as input
            (the project name, the source directory full path, and the source
            filename) and returns any structure. This callback is used to
            populate the leaves of the above tree. Defaults to returning the
            source filename.
        :type transform: Function or None
        :return: a dictionary listing the project sources
        :rtype: dict[str, dict[str, list[*]]]
        """

        def reduce_source_dirs(sources):
            """Compute the list of source directories from source files.

            :param collections.Iterable[str] sources: the list of source files'
                absolute path
            :return: the list of unique source directories' absolute path
            :rtype: collections.Iterable[str]
            """

            return list(set((
                os.path.normpath(os.path.dirname(f)) for f in sources
            )))

        transform = transform or (
            # Default to returning the filename if |transform| is not provided
            lambda project_name, source_dir, filename: filename
        )

        # TODO(delay): it might be worth considering exporting this function
        # or a more generic version of it from the GNAThub module.
        return {
            project: {
                directory: [
                    transform(project, directory, os.path.basename(path))
                    for path in sources
                    if os.path.normpath(os.path.dirname(path)) == directory
                ] for directory in reduce_source_dirs(sources)
            }
            for project, sources in GNAThub.Project.source_files().iteritems()
            if sources
        }

    @classmethod
    def _decorate_dict(cls, obj, extra=None):
        """Decorate a Python dictionary with additional properties.

        :param dict[str, *] obj: the Python dictionary to decorate
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str, *]
        """

        if extra:
            obj.update(extra)
        return obj

    @classmethod
    def _encode_tool(cls, tool, extra=None):
        """JSON-encode a tool.

        :param GNAThub.Tool tool: the tool to encode
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str, *]
        """

        return cls._decorate_dict({
            'id': tool.id,
            'name': tool.name
        }, extra)

    @classmethod
    def _encode_rule(cls, rule, tool, extra=None):
        """JSON-encode a rule.

        :param GNAThub.Rule rule: the rule to encode
        :param GNAThub.Tool tool: the tool associated with the rule
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str, *]
        """

        return cls._decorate_dict({
            'id': rule.id,
            'identifier': rule.identifier,
            'name': rule.name,
            'kind': rule.kind,
            'tool': cls._encode_tool(tool)
        }, extra)

    @classmethod
    def _encode_message_property(cls, prop, extra=None):
        """JSON-encode a message property.

        :param GNAThub.Property prop: the property associated with the message
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str,*]
        """

        return cls._decorate_dict({
            'id': prop.id,
            'identifier': prop.identifier,
            'name': prop.name
        }, extra)

    @classmethod
    def _encode_message(cls, msg, rule, tool, extra=None):
        """JSON-encode a message.

        :param GNAThub.Message msg: the message to encode
        :param GNAThub.Rule rule: the rule associated with the message
        :param GNAThub.Tool tool: the tool associated with the rule
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str, *]
        """

        return cls._decorate_dict({
            'begin': msg.col_begin,
            'end': msg.col_end,
            'rule': cls._encode_rule(rule, tool),
            'properties': [
                cls._encode_message_property(prop)
                for prop in msg.get_properties()
            ],
            'message': msg.data
        }, extra)

    def _encode_coverage(cls, msg, tool, extra=None):
        """JSON-encode coverage information.

        :param GNAThub.Message msg: the message to encode
        :param GNAThub.Tool tool: the coverage tool that generated this message
        :param extra: extra fields to decorate the encoded object with
        :type extra: dict or None
        :rtype: dict[str, *]
        """

        hits, status = -1, CoverageStatus.NO_CODE
        if tool.name == 'gcov':
            hits = int(msg.data)
            status = (
                CoverageStatus.COVERED if hits else CoverageStatus.NOT_COVERED)
        return cls._decorate_dict({
            'status': status.name,
            'hits': hits
        }, extra)

    def generate_src_hunk(self, project_name, source_file):
        """Generate the JSON-encoded representation of `source_file`

        :param str project_name: the name of the project the source if from
        :param str source_file: the full path to the source file
        :return: the JSON-encoded representation of `source_file`
        :rtype: dict[str, *]
        :raises: IOError
        """

        assert os.path.isfile(source_file), '{}: not such file ({})'.format(
            os.path.basename(source_file), source_file
        )

        messages_from_db = GNAThub.Resource.get(source_file).list_messages()

        tools, rules, properties = {}, {}, {}
        messages = collections.defaultdict(list)
        coverage = collections.defaultdict(str)

        for msg in messages_from_db:
            rule = self._rules_by_id[msg.rule_id]
            tool = self._tools_by_id[rule.tool_id]

            for prop in msg.get_properties():
                if prop.id not in properties:
                    properties[prop.id] = self._encode_message_property(prop, {
                        'message_count': 1
                    })
                else:
                    properties[prop.id]['message_count'] += 1

            if msg.line != 0:
                # Increment the count of inline messages
                if tool.id not in tools:
                    tools[tool.id] = self._encode_tool(tool, {
                        'message_count': 1
                    })
                else:
                    tools[tool.id]['message_count'] += 1
                if rule.id not in rules:
                    rules[rule.id] = self._encode_rule(rule, tool, {
                        'message_count': 1
                    })
                else:
                    rules[rule.id]['message_count'] += 1

            if rule.identifier != 'coverage':
                messages[msg.line].append(
                    self._encode_message(msg, rule, tool))
            else:
                # Only one coverage tool shall be used. The last entry
                # overwrites previous ones.
                coverage[msg.line] = self._encode_coverage(msg, tool)

        src_hunk = {
            'project': project_name,
            'filename': os.path.basename(source_file),
            'metrics': messages[0],
            'properties': properties,
            'tools': tools,
            'rules': rules,
            'lines': None
        }

        try:
            with open(source_file, 'r') as infile:
                content = infile.read()
        except IOError:
            self.log.exception('failed to read source file: %s', source_file)
            self.log.warn('report might be incomplete')
            return src_hunk

        # NOTE: Pygments lexer seems to drop those loading and trailing new
        # lines in its output. Add them back after HTMLization to avoid line
        # desynchronization with the original files.
        raw_lines = content.splitlines()
        lead_nl_count, trail_nl_count = count_extra_newlines(raw_lines)

        # Select the appropriate lexer; fall back on "Null" lexer if no match.
        try:
            lexer = pygments.lexers.guess_lexer_for_filename(
                source_file, content)
        except pygments.util.ClassNotFound:
            self.log.warn('could not guess lexer from file: %s', source_file)
            self.log.warn('fall back to using TextLexer (ie. no highlighting)')
            lexer = pygments.lexers.special.TextLexer()

        # Custom HTML formatter outputting the decorated source code as a DOM.
        formatter = _HtmlFormatter()

        try:
            decoded_raw_lines = [line.decode('utf-8') for line in raw_lines]
            assert len(decoded_raw_lines) == len(raw_lines)
        except UnicodeDecodeError:
            self.log.exception('failed to decode UTF-8: %s', source_file)
            self.log.warn('source file content will not be available')
            decoded_raw_lines = None

        # Attempt to highligth the source file; fall back on raw on failure.
        try:
            highlighted = (
                [''] * lead_nl_count +
                highlight(content, lexer, formatter).splitlines() +
                [''] * trail_nl_count
            )

            assert len(raw_lines) == len(highlighted), ' '.join([
                'mismatching number of source line in the HTML output;',
                'expected {}, got {}'.format(len(raw_lines), len(highlighted))
            ])

            src_hunk['lines'] = [{
                'number': no,
                'content': decoded_raw_lines[no - 1],
                'html_content': (
                    highlighted[no - 1]
                    if highlighted and len(highlighted) > no else None
                ),
                'coverage': coverage[no],
                'messages': messages[no]
            } for no in range(1, len(raw_lines) + 1)]
        except Exception:
            self.log.exception(
                'unhandled exception during HTML generation: %s', source_file)
            self.log.warn('report might be incomplete')
        return src_hunk

    def generate_index(self):
        """Generate the report index.

        The index contains top-level information such as project name, database
        location, ... as well as the list of sub-project and their source
        directories and source files, along with some important metrics.

        :return: the JSON-encoded index
        :rtype: dict[str, *]
        """

        """Map tool ID to number of message generated project-wide

        :type: dict[int, int]
        """
        project_msg_count = collections.defaultdict(int)

        def _aggregate_metrics(project, source_dir, filename):
            """Collect metrics about the given source file.

            :param str project: the name of the project the source belongs to
            :param str source_dir: the full path to the source directory
                containing the source file
            :param str filename: the basename of the source file
            :return: the dictionary containing metrics for `filename`
            :rtype: dict[str, *]
            """

            # Computes file-level metrics.
            resource = GNAThub.Resource.get(os.path.join(source_dir, filename))
            metrics, msg_count = [], collections.defaultdict(int)
            if resource:
                for msg in resource.list_messages():
                    rule = self._rules_by_id[msg.rule_id]
                    # Note: the DB schema is currently designed so that metrics
                    # are stored has messages, and file metrics are attached to
                    # line 0.
                    if msg.line == 0:
                        metrics.append(self._encode_message(
                            msg, rule, self._tools_by_id[rule.tool_id]))
                    elif rule.identifier != 'coverage':
                        msg_count[rule.tool_id] += 1

            # Report sums at the project level.
            for tool_id in msg_count.iterkeys():
                project_msg_count[tool_id] += msg_count[tool_id]

            return {
                'filename': filename,
                'metrics': metrics or None,
                'message_count': msg_count or None,
                '_associated_resource': resource is not None
            }

        def _restructure_module(name, module):
            """Transform the module tree.

            :param str name: the name of the module
            :param dict[str, list[*]] module: the module dictionary to
                transform
            :return: the newly restructured module
            :rtype: dict[str, *]
            """

            source_dirs, module_msg_count = {}, collections.defaultdict(int)

            for source_dir, sources in module.iteritems():
                source_dir_msg_count = collections.defaultdict(int)
                for source in sources:
                    if source['message_count']:
                        for tool_id, count in (
                            source.get('message_count', {}).iteritems()
                        ):
                            source_dir_msg_count[tool_id] += count
                            module_msg_count[tool_id] += count
                source_dirs[source_dir] = {
                    'name': source_dir,
                    'sources': sources,
                    'message_count': source_dir_msg_count or None
                }

            paths = module.keys()
            return {
                'name': name,
                'source_dirs': source_dirs,
                'message_count': module_msg_count or None,
                '_source_dirs_common_prefix': (
                    os.path.commonprefix(paths) if len(paths) > 1
                    else os.path.dirname(paths[0])
                )
            }

        return {
            'modules': {
                name: _restructure_module(name, module) for name, module
                in self._generate_source_tree(_aggregate_metrics).iteritems()
            },
            'project': GNAThub.Project.name(),
            'creation_time': int(time.time()),
            'tools': {
                id: self._encode_tool(tool)
                for id, tool in self._tools_by_id.iteritems()
            },
            'rules': {
                id: self._encode_rule(rule, self._tools_by_id[rule.tool_id])
                for id, rule in self._rules_by_id.iteritems()
            },
            'properties': [
                self._encode_message_property(prop)
                for prop in GNAThub.Property.list()
            ],
            'message_count': project_msg_count,
            '_database': GNAThub.database()
        }
