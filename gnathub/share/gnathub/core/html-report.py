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

    """
    A dictionary of :class:`GNAThub.Tool` indexed by tool name (in lowercase)
    and used for constant access.

    This dictionnary is lazily computed by the :meth:`_get_tool_by_name`
    method.

    :type: dict[str,GNAThub.Tool] | None
    """
    _tools_by_name = None

    """
    A dictionary of :class:`GNAThub.Tool` indexed by tool ID and used for
    constant access.

    This dictionnary is lazily computed by the :meth:`_get_tool_by_id` method.

    :type: dict[number,GNAThub.Tool] | None
    """
    _tools_by_id = None

    def setup(self):
        super(HTMLReport, self).setup()

    @property
    def name(self):
        return 'html-report'

    def _get_tool_by_id(self, id):
        """Return the Tool object matching `id` if it was executed.

        Returns `None` if the tool was not executed.

        :param name: the ID of the tool
        :type name: number
        :rtype: GNAThub.Tool | None
        """
        if self._tools_by_id is None:
            # Cache the list of tools for future lookups
            self._tools_by_id = {tool.id: tool for tool in GNAThub.Tool.list()}
        return self._tools_by_id.get(id)

    def _get_tool_by_name(self, name):
        """Return the Tool object matching `name` if it was executed.

        Returns `None` if the tool was not executed.

        :param name: the name of the tool
        :type name: str
        :rtype: GNAThub.Tool | None
        """
        if self._tools_by_name is None:
            # Cache the list of tools for future lookups
            self._tools_by_name = {
                tool.name.lower(): tool for tool in GNAThub.Tool.list()
            }
        return self._tools_by_name.get(name.lower())

    @property
    def output_dir(self):
        """Return the path to the directory where to generate the HTML report

        :return: the full path to the output directory
        :rtype: str
        """

        return os.path.join(GNAThub.root(), self.name)

    def _json_dump(self, output, obj, indent=None):
        """Dump a JSON-encoded representation of `obj` into `output`

        :param output: path to the output file
        :type output: str
        :param obj: object to serialize and save into `output`
        :type obj: dict | list | str | int
        :raise: IOError
        """

        self.log.debug('generating %s', output)
        with open(output, 'w') as outfile:
            outfile.write(json.dumps(obj, indent=indent))

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

            set = {}
            map(set.__setitem__, [
                os.path.normpath(os.path.dirname(f)) for f in sources
            ], [])
            return set.keys()

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

    def _generate_report_src_hunk(self, source_file):
        """Generate the JSON-encoded representation of `source_file`

        :param source_file: the full path to the source file
        :type source_file: str
        :return: the JSON-encoded representation of `source_file`
        :rtype: dict[str,*]
        :raise: IOError
        """

        assert os.path.isfile(source_file), '{}: not such file ({})'.format(
            os.path.basename(source_file), source_file
        )

        with open(source_file, 'r') as infile:
            return {
                'filename': os.path.basename(source_file),
                'lines': [{
                    'number': no,
                    'content': line
                } for no, line in enumerate(infile, start=1)]
            }

    def _generate_report_index(self):
        """Generate the report index

        The index contains top-level information such as project name, database
        location, ... as well as the list of sub-project and their source
        directories and source files, along with some important metrics.

        :return: the
        :rtype: dict[str,*]
        """

        # Get the GNATmetric tool definition
        GNATMETRIC = self._get_tool_by_name('GNATmetric')

        # Generate the dictionary of rules
        RULES = {rule.id: rule for rule in GNAThub.Rule.list()}

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
            metrics = {
                RULES[msg.rule_id].name: float(msg.data)
                for msg in (
                    resource.list_messages() if resource else []
                ) if RULES[msg.rule_id].tool_id == GNATMETRIC.id
            } if GNATMETRIC else None

            return {
                'filename': filename,
                'partname': '{}.json'.format(filename),
                'metrics': metrics,
                '_associated_resource': resource is not None
            }

        return {
            'modules': self._generate_source_tree(_aggregate_metrics),
            'project': GNAThub.Project.name(),
            '_database': GNAThub.database()
        }

    def execute(self):
        """Generates JSON-encoded representation of the data collected"""

        self.info('generate JSON-encoded report')

        try:
            if not os.path.exists(self.output_dir):
                os.makedirs(self.output_dir)
            else:
                self.log.warn('%s: already exists', self.output_dir)
                self.log.warn('existing report may be overriden')

            # Generate the report index
            index = self._generate_report_index()

            # Compute the total source file count to display execution progress
            # Note: this is a more efficient version of:
            #
            #   count, total = 0, 1
            #   for source_dirs in index['modules'].values():
            #       for source_hunks in source_dirs.values():
            #           total += len(source_hunks)
            #
            # Using generators and the built-in sum function, the following
            # code ensures the smaller memory footprint and the best
            # opportunities for the Python VM to optimize the count
            # computation.
            count, total = 0, sum((sum((
                len(source_hunks) for source_hunks in source_dirs.itervalues()
            )) for source_dirs in index['modules'].itervalues())) + 1

            # Serialize each source of the project
            for _, source_dirs in index['modules'].iteritems():
                for source_dir, source_hunks in source_dirs.iteritems():
                    for source in source_hunks:
                        output_hunk_path = os.path.join(
                            self.output_dir, source['partname']
                        )
                        self._json_dump(
                            output_hunk_path,
                            self._generate_report_src_hunk(
                                os.path.join(source_dir, source['filename'])
                            )
                        )
                        count = count + 1
                        assert count != total, 'internal error'
                        Console.progress(count, total, False)
                        self.log.debug('hunk written to %s', output_hunk_path)

            # Serialize the index
            output_index_path = os.path.join(
                self.output_dir,
                '{}.report.json'.format(GNAThub.Project.name().lower())
            )
            self._json_dump(output_index_path, index)
            assert count + 1 == total, 'internal error'
            Console.progress(count + 1, total, True)
            self.info('report written to %s' % output_index_path)

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate JSON files')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
