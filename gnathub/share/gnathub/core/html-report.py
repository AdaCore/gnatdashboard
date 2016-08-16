# GNAThub (GNATdashboard)
# Copyright (C) 2016, AdaCore
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

"""GNAThub plug-in for the generation of a standalone rich HTML report

It exports the HTMLReport class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

import collections
import json
import inspect
import os
import time

from GNAThub import Console
from shutil import copy2, copytree, rmtree


class HTMLReport(GNAThub.Plugin):
    """HTMLReport plugin for GNAThub"""

    def __init__(self):
        super(HTMLReport, self).__init__()
        self._rules_by_id = None
        self._tools_by_id = None
        self._tools_by_name = None

    @property
    def name(self):
        return 'html-report'

    @property
    def webapp_dir(self):
        """Return the path to the webapp directory.

        This directory contains the generic parts of the web application.

        :rtype: str
        """
        this = inspect.getfile(inspect.currentframe())
        return os.path.join(os.path.dirname(os.path.dirname(this)), 'webui')

    def setup(self):
        """Inherited."""
        super(HTMLReport, self).setup()
        rules = GNAThub.Rule.list()
        tools = GNAThub.Tool.list()
        self._rules_by_id = {rule.id: rule for rule in GNAThub.Rule.list()}
        self._tools_by_id = {tool.id: tool for tool in tools}
        self._tools_by_name = {tool.name.lower(): tool for tool in tools}

    @property
    def output_dir(self):
        """Return the path to the directory where to generate the HTML report

        :return: the full path to the output directory
        :rtype: str
        """

        return os.path.join(GNAThub.root(), self.name)

    def _write_json(self, output, obj, **kwargs):
        """Dump a JSON-encoded representation of `obj` into `output`

        :param output: path to the output file
        :type output: str
        :param obj: object to serialize and save into `output`
        :type obj: dict | list | str | int
        :param kwargs: the parameters to pass to the underlying
            :func:`json.dumps` function; see :func:`json.dumps` documentation
            for more information
        :type kwargs: dict
        :raise: IOError
        :see: :func:`json.dumps`
        """

        self.log.debug('generating %s', output)
        with open(output, 'w') as outfile:
            outfile.write(json.dumps(obj, **kwargs))

    def _generate_source_tree(self, transform=None):
        """Generate the initial project structure

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
            populate the lists at the bottom of the above structure.  Defaults
            to returning the source filename.
        :type transform: Function | None
        :return: a dictionary listing the project sources
        :rtype: dict[str,dict[str,list[str]]]
        """

        def reduce_source_dirs(sources):
            """Compute the list of source directories from source files

            :param sources: the list of source files' absolute path
            :type sources: list[str]
            :return: the list of unique source directories' absolute path
            :rtype: list[str]
            """

            return list(set((
                os.path.normpath(os.path.dirname(f)) for f in sources
            )))

        transform = transform or (
            # Default to returning the filename if |transform| is not provided
            lambda project_name, source_dir, filename: filename
        )

        # TODO(charly): it might be worth considering exporting this function
        # or a more generic version of it from the GNAThub module.
        return {
            project: {
                directory: [
                    transform(project, directory, os.path.basename(path))
                    for path in sources
                    if os.path.normpath(os.path.dirname(path)) == directory
                ] for directory in reduce_source_dirs(sources)
            }
            for project, sources in GNAThub.Project.source_files().items()
            if sources
        }

    @classmethod
    def _decorate_dict(cls, obj, **kwargs):
        """Decorate a Python dictionary with additional properties

        :param obj: the Python dictionary to decorate
        :type obj: dict[str,*]
        :param kwargs: extra fields to decorate the encoded object with
        :type kwargs: dict | None
        :rtype: dict[str,*]
        """
        if kwargs:
            obj.update(kwargs)
        return obj

    @classmethod
    def _encode_tool(cls, tool, **kwargs):
        """JSON-encode a tool

        :param tool: the tool to encode
        :type tool: GNAThub.Tool
        :param kwargs: extra fields to decorate the encoded object with
        :type kwargs: dict | None
        :rtype: dict[str,*]
        """

        return cls._decorate_dict({
            'id': tool.id,
            'name': tool.name
        }, **kwargs)

    @classmethod
    def _encode_rule(cls, rule, tool, **kwargs):
        """JSON-encode a rule

        :param rule: the rule to encode
        :type rule: GNAThub.Rule
        :param tool: the tool associated with the rule
        :type tool: GNAThub.Tool
        :param kwargs: extra fields to decorate the encoded object with
        :type kwargs: dict | None
        :rtype: dict[str,*]
        """

        return cls._decorate_dict({
            'identifier': rule.identifier,
            'name': rule.name,
            'kind': rule.kind,
            'tool': cls._encode_tool(tool)
        }, **kwargs)

    @classmethod
    def _encode_message(cls, msg, rule, tool, **kwargs):
        """JSON-encode a message

        :param msg: the message to encode
        :type msg: GNAThub.Message
        :param rule: the rule associated with the message
        :type rule: GNAThub.Rule
        :param tool: the tool associated with the rule
        :type tool: GNAThub.Tool
        :param kwargs: extra fields to decorate the encoded object with
        :type kwargs: dict | None
        :rtype: dict[str,*]
        """

        return cls._decorate_dict({
            'begin': msg.col_begin,
            'end': msg.col_end,
            'rule': cls._encode_rule(rule, tool),
            'message': msg.data
        }, **kwargs)

    def _generate_report_src_hunk(self, project_name, source_file):
        """Generate the JSON-encoded representation of `source_file`

        :param project_name: the name of the project the source if from
        :type project_name: str
        :param source_file: the full path to the source file
        :type source_file: str
        :return: the JSON-encoded representation of `source_file`
        :rtype: dict[str,*]
        :raise: IOError
        """

        assert os.path.isfile(source_file), '{}: not such file ({})'.format(
            os.path.basename(source_file), source_file
        )

        messages_from_db = GNAThub.Resource.get(source_file).list_messages()

        messages = collections.defaultdict(list)
        coverage = collections.defaultdict(str)
        for msg in messages_from_db:
            rule = self._rules_by_id[msg.rule_id]
            tool = self._tools_by_id[rule.tool_id]
            if rule.identifier != 'coverage':
                messages[msg.line].append(
                    self._encode_message(msg, rule, tool)
                )
            elif tool.name == 'gcov':
                coverage[msg.line] = (
                    'COVERED' if int(msg.data) else 'NOT_COVERED'
                )
            else:
                pass  # TODO(delay): add support for GNATcoverage

        src_hunk = {
            'project': project_name,
            'filename': os.path.basename(source_file),
            'metrics': messages[0],
            'tools': {
                tool.id: self._encode_tool(tool, message_count=len([
                    msg for msg in messages_from_db if (
                        self._rules_by_id[msg.rule_id].tool_id == tool.id and
                        msg.line != 0
                    )
                ])) for tool in self._tools_by_id.itervalues()
            },
            'lines': None
        }

        try:
            with open(source_file, 'r') as infile:
                src_hunk['lines'] = [{
                    'number': no,
                    'content': line,
                    'coverage': coverage[no],
                    'messages': messages[no]
                } for no, line in enumerate(infile, start=1)]
        except IOError:
            self.log.exception('failed to read source file: %s', source_file)
            self.warn('failed to read source file: %s', source_file)
            self.warn('report might be incomplete')
        finally:
            return src_hunk

    def _generate_report_index(self):
        """Generate the report index

        The index contains top-level information such as project name, database
        location, ... as well as the list of sub-project and their source
        directories and source files, along with some important metrics.

        :return: the JSON-encoded index
        :rtype: dict[str,*]
        """

        def _aggregate_metrics(project, source_dir, filename):
            """Collect metrics about the given source file

            :param project: the name of the project the source belongs to
            :type project: str
            :param source_dir: the full path to the source directory containing
                the source file
            :type source_dir: str
            :param filename: the basename of the source file
            :type filename: str
            :return: the dictionary containing metrics for `filename`
            :rtype: dict[str,*]
            """

            # Computes file-level metrics
            resource = GNAThub.Resource.get(os.path.join(source_dir, filename))
            metrics, msg_count = [], 0
            if resource:
                for msg in resource.list_messages():
                    rule = self._rules_by_id[msg.rule_id]
                    tool = self._tools_by_id[rule.tool_id]
                    # Note: the DB schema is currently designed so that metrics
                    # are stored has messages, and file metrics are attached to
                    # line 0.
                    if msg.line == 0:
                        metrics.append(self._encode_message(msg, rule, tool))
                    elif rule.identifier != 'coverage':
                        msg_count += 1

            return {
                'filename': filename,
                'metrics': metrics or None,
                'message_count': msg_count,
                '_associated_resource': resource is not None
            }

        return {
            'modules': self._generate_source_tree(_aggregate_metrics),
            'project': GNAThub.Project.name(),
            'creation_time': int(time.time()),
            'tools': [
                self._encode_tool(tool)
                for tool in self._tools_by_id.itervalues()
            ],
            'rules': [
                self._encode_rule(rule, self._tools_by_id[rule.tool_id])
                for rule in self._rules_by_id.itervalues()
            ],
            '_database': GNAThub.database()
        }

    def execute(self):
        """Generates JSON-encoded representation of the data collected"""

        # The output directory for the JSON-encoded report data
        data_output_dir = os.path.join(self.output_dir, 'data')
        data_src_output_dir = os.path.join(data_output_dir, 'src')

        def _write_report_src_hunk(project, source_dir, source):
            """Write a report source hunk to disk

            :param project: the name of the project
            :type project: str
            :param source_dir: the source directory in which to find the source
            :type source_dir: str
            :param source: the source metadata (as generated by the method
                :meth:`_generate_report_index`)
            :type source: dict[str,*]
            """

            def _fname(ext):
                return '{}.{}'.format(os.path.join(
                    data_src_output_dir, source['filename']), ext)

            src_hunk = self._generate_report_src_hunk(
                project, os.path.join(source_dir, source['filename']))
            self._write_json(_fname('json'), src_hunk, indent=2)
            self.log.debug('src hunk written to %s', _fname('json'))

        def _write_report_index(project_name, index):
            """Write the report index to disk

            :param project_name: the name of the project
            :type project_name: str
            """

            def _fname(ext):
                return '{}.report.{}'.format(
                    os.path.join(data_output_dir, project_name.lower()), ext)

            self._write_json(_fname('json'), index, indent=2)
            self.log.debug('report index written to %s', _fname('json'))

        try:
            self.info('generate JSON-encoded report')

            # Create directory structure if needed
            if not os.path.exists(self.output_dir):
                os.makedirs(self.output_dir)
            else:
                self.log.warn('%s: already exists', self.output_dir)
                self.log.warn('existing report may be overriden')

            # Copy the generic web application files
            for entry in os.listdir(self.webapp_dir):
                path = os.path.join(self.webapp_dir, entry)
                dest = os.path.join(self.output_dir, entry)
                if os.path.isdir(path):
                    if os.path.isdir(dest):
                        rmtree(dest)
                    copytree(path, dest)
                else:
                    copy2(path, dest)

            # Create the JSON-encoded report output directory
            for directory in (data_output_dir, data_src_output_dir):
                if not os.path.exists(directory):
                    os.makedirs(directory)

            # Generate the report index
            report_index = self._generate_report_index()

            # Compute the total source file count to display execution progress
            # Note: this is a more efficient version of:
            #
            #   count, total = 0, 1
            #   for source_dirs in report_index['modules'].values():
            #       for source_hunks in source_dirs.values():
            #           total += len(source_hunks)
            #
            # Using generators and the built-in sum function, the following
            # code ensures the smaller memory footprint and the best
            # opportunities for the Python VM to optimize the count
            # computation.
            count, total = 0, sum((sum((
                len(source_hunks) for source_hunks in source_dirs.itervalues()
            )) for source_dirs in report_index['modules'].itervalues())) + 1

            # Serialize each source of the project
            for project, source_dirs in report_index['modules'].iteritems():
                for source_dir, source_hunks in source_dirs.iteritems():
                    for src_hunk in source_hunks:
                        _write_report_src_hunk(project, source_dir, src_hunk)
                        count = count + 1
                        assert count != total, 'internal error'
                        Console.progress(count, total, False)

            # Serialize the index
            _write_report_index(GNAThub.Project.name(), report_index)
            assert count + 1 == total, 'internal error'
            Console.progress(count + 1, total, True)
            self.info('report written to %s', self.output_dir)

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate the HTML report')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
