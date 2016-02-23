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

"""???

???
"""

import GNAThub

import collections
import json
import os


class HTMLReport(GNAThub.Plugin):
    """SonarConfig plugin for GNAThub"""

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

    def _main_as_json(self, modules):
        """???

        :param modules: project modules and their associated source directories
        :type modules: dict[str,list[str]]
        :return: ???
        :rtype: ???
        """

        main = {
            'modules': collections.defaultdict(
                lambda: collections.defaultdict(
                    lambda: collections.defaultdict(lambda: []))),
            'project': GNAThub.Project.name(),
            '_database': GNAThub.database()
        }

        for project, source_dirs in modules.iteritems():
            dirs_commonprefix = os.path.commonprefix(source_dirs)
            self.log.info('source dirs common prefix: %s', dirs_commonprefix)
            main['modules'][project]['part_dir'] = '{}-part'.format(
                project.lower())

            for source_dir in source_dirs:
                src_dir_relpath = os.path.relpath(source_dir,
                                                  dirs_commonprefix)

                if not os.path.isdir(source_dir):
                    self.log.warn('%s: no such directory', source_dir)
                    continue

                for source in os.listdir(source_dir):
                    source_path = os.path.join(source_dir, source)
                    if not os.path.isfile(source_path):
                        continue
                    partname = os.path.normpath(
                        os.path.join(src_dir_relpath, '{}.part'.format(source))
                    )

                    # Create a dictionary of tools
                    tools = {tool.id: tool for tool in GNAThub.Tool.list()}
                    assert len(GNAThub.Tool.list()) == 2, GNAThub.Tool.list()

                    gnatmetric = self._get_tool_by_name('GNATmetric')
                    assert gnatmetric, 'GNATmetric results not found'

                    # Create a dictionary of rules
                    rules = {rule.id: rule for rule in GNAThub.Rule.list()}

                    resource = GNAThub.Resource.get(source_path)
                    metrics = {
                        rules[msg.rule_id].name: float(msg.data)
                        for msg in (resource.list_messages()
                                    if resource else [])
                        if rules[msg.rule_id].tool_id == gnatmetric.id
                    }

                    main['modules'][project]['sources'][source_dir].append({
                        'filename': source,
                        'partname': partname,
                        'metrics': metrics,
                        '_associated_resource': resource is not None
                    })

        return main

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

    def _generate_sources_as_json(self, modules):
        """Generate the JSON-encoded reports

        :param modules: project modules and their associated source directories
        :type modules: dict[str,list[str]]
        :return: the path to the root source directory and a copy of the input
            ``modules`` directory with updated path to source directories
            (pointing to the local copy)
        :rtype: (str, dict[str,list[str]])
        """

        # Compute the total dirs count to copy to display progress
        count = 0
        total = sum([len(dirs) for dirs in modules.itervalues()])

        root_src_dir = SonarQube.src_cache()

        self.info('generate JSON report for HTML output')
        self.log.info(
            'generating JSON reports from the project closure to %s',
            os.path.relpath(root_src_dir))

        # Remove any previous analysis left-over
        shutil.rmtree(root_src_dir, ignore_errors=True)
        new_modules_mapping = collections.OrderedDict()

        for module_name in modules:
            module_root_src_dir = os.path.join(root_src_dir, module_name)
            new_modules_mapping[module_name] = _escpath(module_root_src_dir)

            module_root_src_dir = os.path.join(module_root_src_dir,
                                               module_name.lower() + '-report')

            if not os.path.exists(module_root_src_dir):
                os.makedirs(module_root_src_dir)

            module_src_dirs = modules[module_name]
            dirs_commonprefix = os.path.commonprefix(module_src_dirs)
            self.log.info('source dirs common prefix: %s', dirs_commonprefix)

            self.info('prepare files from module: %s' % module_name)
            for src_dir in modules[module_name]:
                src_dir_relpath = os.path.relpath(src_dir, dirs_commonprefix)
                src_dir_path = os.path.join(module_root_src_dir,
                                            src_dir_relpath)
                self.log.info(' + %s' % src_dir_path)

                if not os.path.exists(src_dir_path):
                    os.makedirs(src_dir_path)

                # Generate JSON-encoded reports
                for entry in os.listdir(src_dir):
                    entry_path = os.path.join(src_dir, entry)

                    if not os.path.isfile(entry_path):
                        continue

                    new_path = os.path.join(src_dir_path, entry)
                    self.log.debug('%s -> %s', entry_path, new_path)

                    # TODO(delay): generate JSON report

                    self.src_mapping[_escpath(entry_path)] = \
                        _escpath(os.path.normpath(new_path))

                count = count + 1
                Console.progress(count, total, count == total)

        return root_src_dir, new_modules_mapping

    def execute(self):
        """Generates JSON-encoded representation of the data collected"""

        self.info('generate JSON-encoded reports')

        # source_dirs = GNAThub.Project.source_dirs()[project_name]
        modules = {k: v for k, v in GNAThub.Project.source_dirs().items() if v}

        try:
            print json.dumps(self._main_as_json(modules), indent=2)
            self.exec_status = GNAThub.EXEC_SUCCESS
            return
            with open('main.json', 'w') as outfile:
                outfile.write(
                    json.dumps(self._main_as_json(modules), indent=2))

            for source in self._sources_as_json(modules):
                output_path = '{}.json'.format(
                    os.path.join(os.path.basename(source['path'])))
                self.log.debug('Generating %s', output_path)
                with open(output_path, 'w') as outfile:
                    outfile.write(json.dumps(source, indent=2))

        except IOError as why:
            self.exec_status = GNAThub.EXEC_FAILURE
            self.log.exception('failed to generate JSON files')
            self.error(str(why))

        else:
            self.exec_status = GNAThub.EXEC_SUCCESS
