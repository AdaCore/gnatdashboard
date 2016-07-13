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
import os

from GNAThub import Console


class HTMLReport(GNAThub.Plugin):
    """HTMLReport plugin for GNAThub"""

    def __init__(self):
        super(HTMLReport, self).__init__()
        self._rules = None
        self._tools_by_id = None
        self._tools_by_name = None

    @property
    def name(self):
        return 'html-report'

    def setup(self):
        """Inherited."""
        super(HTMLReport, self).setup()
        rules = GNAThub.Rule.list()
        tools = GNAThub.Tool.list()
        self._rules = {rule.id: rule for rule in GNAThub.Rule.list()}
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

    def _write_js(self, output, obj, js_wrapper_name, **kwargs):
        """Generate a JavaScript file wrapping `obj` in `js_wrapper_name`

        `obj` is JSON-encoded and used as argument of `js_wrapper_name`. This
        function must be provided by the web application and used to load the
        data.

        :param output: path to the output file
        :type output: str
        :param obj: object to serialize and save into `output`
        :type obj: dict | list | str | int
        :param js_wrapper_name: the name of the JavaScript function to call
            with `obj` as parameter
        :type js_wrapper_name: str
        :param kwargs: the parameters to pass to the underlying
            :func:`json.dumps` function; see :func:`json.dumps` documentation
            for more information
        :type kwargs: dict
        :raise: IOError
        :see: :func:`json.dumps`
        """

        self.log.debug('generating %s', output)
        with open(output, 'w') as outfile:
            outfile.write('{}({});'.format(
                js_wrapper_name, json.dumps(obj, **kwargs)))

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

        messages = collections.defaultdict(list)
        coverage = collections.defaultdict(str)
        for msg in GNAThub.Resource.get(source_file).list_messages():
            rule = self._rules[msg.rule_id]
            tool = self._tools_by_id[rule.tool_id]
            if rule.identifier != 'coverage':
                messages[msg.line].append({
                    'begin': msg.col_begin,
                    'end': msg.col_end,
                    'rule': {
                        'identifier': rule.identifier,
                        'name': rule.name,
                        'kind': rule.kind,
                        'tool': tool.name,
                    },
                    'message': msg.data
                })
            elif tool.name == 'gcov':
                coverage[msg.line] = (
                    'COVERED' if int(msg.data) else 'NOT_COVERED'
                )
            else:
                pass  # TODO(delay): add support for GNATcoverage

        with open(source_file, 'r') as infile:
            return {
                'project': project_name,
                'filename': os.path.basename(source_file),
                'metrics': messages[0],
                'lines': [{
                    'number': no,
                    'content': line,
                    'coverage': coverage[no],
                    'messages': messages[no]
                } for no, line in enumerate(infile, start=1)]
            }

    def _generate_report_index(self):
        """Generate the report index

        The index contains top-level information such as project name, database
        location, ... as well as the list of sub-project and their source
        directories and source files, along with some important metrics.

        :return: the JSON-encoded index
        :rtype: dict[str,*]
        """

        # Get the GNATmetric tool definition
        gnatmetric = self._tools_by_name.get('GNATmetric')

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

            resource = GNAThub.Resource.get(os.path.join(source_dir, filename))
            return {
                'filename': filename,
                'metrics':  {
                    self._rules[msg.rule_id].name: float(msg.data)
                    for msg in (
                        resource.list_messages() if resource else []
                    ) if self._rules[msg.rule_id].tool_id == gnatmetric.id
                } if gnatmetric else None,
                '_associated_resource': resource is not None
            }

        return {
            'modules': self._generate_source_tree(_aggregate_metrics),
            'project': GNAThub.Project.name(),
            '_database': GNAThub.database()
        }

    def execute(self):
        """Generates JSON-encoded representation of the data collected"""

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
                return '{}.{}'.format(
                    os.path.join(self.output_dir, source['filename']), ext)

            src_hunk = self._generate_report_src_hunk(
                project, os.path.join(source_dir, source['filename']))
            self._write_json(_fname('json'), src_hunk, indent=2)
            self.log.debug('src hunk written to %s', _fname('json'))
            self._write_js(_fname('js'), src_hunk, 'load_src_hunk')
            self.log.debug('src hunk written to %s', _fname('js'))

        def _write_report_index(project_name, index):
            """Write the report index to disk

            :param project_name: the name of the project
            :type project_name: str
            """

            def _fname(ext):
                return '{}.report.{}'.format(
                    os.path.join(self.output_dir, project_name.lower()), ext)

            self._write_json(_fname('json'), index, indent=2)
            self.log.debug('report index written to %s', _fname('json'))
            self._write_js(_fname('js'), index, 'load_index')
            self.log.debug('report index written to %s', _fname('js'))

        try:
            self.info('generate JSON-encoded report')
            if not os.path.exists(self.output_dir):
                os.makedirs(self.output_dir)
            else:
                self.log.warn('%s: already exists', self.output_dir)
                self.log.warn('existing report may be overriden')

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
            self.info('report written to {}'.format(self.output_dir))

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate the HTML report')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
