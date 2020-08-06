# GNAThub (GNATdashboard)
# Copyright (C) 2013-2020, AdaCore
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

"""Helper module.

Factorize shared components between SonarConfig and SonarScanner.
"""

import collections
import logging
import os
import shutil

import GNAThub
from GNAThub import Console


def _escpath(path):
    """Escape the given path to avoid issues on Windows."""
    return path.replace('\\', '\\\\')


class SonarQube(object):
    """SonarQube helper class.

    Provides a set of helper static methods used by both the SonarScanner
    plug-in and the SonarConfig plug-in.
    """

    EXEC_DIRECTORY = 'sonar'
    CONFIGURATION = 'sonar-project.properties'

    SOURCE_CACHE = 'sources.cache'
    SOURCE_MAPPING = 'sources-mapping.properties'

    @staticmethod
    def workdir():
        """Return the path to sonar execution directory.

        Located within GNAThub's root directory:

            :file:`<project_object_dir>/gnathub/sonar`

        :return: the path to the working directory
        :rtype: str
        """
        return os.path.join(GNAThub.root(), SonarQube.EXEC_DIRECTORY)

    @staticmethod
    def configuration():
        """Return the path to the SonarQube Scanner configuration file.

        Located in the SonarQube-specific directory:

            :file:`<project_object_dir>/gnathub/sonar/sonar-project.properties`

        :return: the path to the configuration file
        :rtype: str
        """
        return os.path.join(SonarQube.workdir(), SonarQube.CONFIGURATION)

    @staticmethod
    def src_mapping():
        """Return the path to the mapping file.

        This file associates original sources path with the equivalent source
        in the local cache:

          :file:`<project_object_dir>/gnathub/sonar/sources-mapping.properties`

        :return: the path to the source mapping file
        :rtype: str
        """
        return os.path.join(SonarQube.workdir(), SonarQube.SOURCE_MAPPING)

    @staticmethod
    def src_cache():
        """Return the path to the local source cache.

        The source cache contains a copy of all analysed sources:

            :file:`<project_object_dir>/gnathub/sonar/sources.cache`

        :return: the path to the source cache
        :rtype: str
        """
        return os.path.join(SonarQube.workdir(), SonarQube.SOURCE_CACHE)

    @staticmethod
    def make_workdir():
        """Create the SonarQube execution directory if it does not exist."""
        if not os.path.exists(SonarQube.workdir()):
            os.makedirs(SonarQube.workdir())


class SonarScannerProperties(object):
    """Builder object for the sonar-scanner configuration file."""

    CONSOLE_NAME = 'sonar-gen-config'

    def __init__(self, logger=None):
        self.log = logger or logging.getLogger(self.__class__.__name__)

        self.attributes = collections.OrderedDict()
        self.src_mapping = collections.OrderedDict()

        # Generate the configuration
        self._generate()

    def info(self, message):
        """Display an informative message.

        :param str message: the message to display
        """
        Console.info(message, prefix=SonarScannerProperties.CONSOLE_NAME)

    def error(self, message):
        """Display an error message.

        :param str message: the message to display
        """
        Console.error(message, prefix=SonarScannerProperties.CONSOLE_NAME)

    @staticmethod
    def _key(key, module=None):
        """Generate the full key.

        :param str key: the property key
        :param module: module to which belongs the key (if ``None``, use the
            default sonar module)
        :type module: str or None
        :return: the key
        :rtype: str
        """
        return '%s.%s' % ('%s.sonar' % module if module else 'sonar', key)

    def _set(self, key, value, module=None):
        """Add a property in the sonar-scanner configuration.

        :param str key: the property key
        :param str value: the property value
        :param module: module to which belongs the key (if ``None``, use the
            default sonar module)
        :type module: str or None
        """
        self.attributes[SonarScannerProperties._key(key, module)] = value

    def _set_dict(self, attributes, module=None):
        """Add multiple properties in the sonar-scanner configuration.

        :param dict[str, str] attributes: collection of attributes to set
        :param module: module to which belongs the key (if ``None``, use the
            default sonar module)
        :type module: str or None
        """
        for key, value in list(attributes.items()):
            self.log.debug('%s = %s', key, value)
            self._set(key, value, module)

    def _set_project_customizable_dict(self, attributes, module=None):
        """Add multiple properties in the sonar-scanner configuration.

        Those properties can be user-customizable via the project file.

        :param attributes: collection of attributes to set
        :type attributes: dict[str, (str, str)]
        :param module: module to which belongs the key (if ``None``, use the
            default sonar module)
        :type module: str or None
        """
        for key, value in list(attributes.items()):
            # Unpack the tuple containing the default value and the custom
            # project attribute for this key.
            value, attribute = value

            assert attribute, 'use SonarScannerProperties._set_dict instead'
            project_property = GNAThub.Project.property_as_string(attribute)

            if project_property:
                self.log.debug('%s = %s (overriding default)', key, value)
            else:
                self.log.debug('%s = %s', key, value)

            self._set(key, project_property or value, module)

    def _get(self, key, module=None):
        """Return the value of a property in the sonar-scanner configuration.

        :param str key: the property key
        :param str module: module to which belongs the key (if ``None``, use
            the default sonar module)
        :return: the value of the property ``key``
        :rtype: str
        """
        return self.attributes.get(SonarScannerProperties._key(key, module))

    def _generate(self):
        """Generate the :file:`sonar-project.properties` configuration.

        Do not create the file yet. See :meth:`write` for this.
        """
        kwargs = {
            'db_path': _escpath(GNAThub.database()),
            'project_name': GNAThub.Project.name(),
            'suffixes': [s[1:] if s.startswith('.') else s
                         for s in GNAThub.Project.source_suffixes('Ada')]
        }

        if len(GNAThub.Project.source_dirs()) == 1:
            self._generate_single_module(**kwargs)
        else:
            self._generate_multi_module(**kwargs)

    @staticmethod
    def _generate_customizable_attr(project_name):
        """Customizable attributes for :file:`sonar-project.properties`.

        :param str project_name: the project name
        :return: the attributes and their value
        :rtype: collections.OrderedDict
        """
        return collections.OrderedDict([
            ('projectName', (project_name, 'Project_Name')),
            ('projectKey', (project_name, 'Project_Key')),
            ('projectVersion', ('unknown', 'Project_Version')),
            ('sourceEncoding', ('UTF-8', 'Source_Encoding'))
        ])

    def _generate_single_module(self, db_path, project_name, suffixes):
        """Generate part of the :file:`sonar-project.properties` configuration.

        Do not create the file yet. See :meth:`write` for this.

        :param str db_path: full path to the DB
        :param str project_name: project name
        :param collections.Iterable[str] suffixes: list of Ada extensions
        """
        source_dirs = GNAThub.Project.source_dirs()[project_name]
        sources, _ = self._generate_source_dirs({project_name: source_dirs})

        non_customizable_attributes = collections.OrderedDict([
            ('language', 'ada'),
            ('sources', _escpath(sources)),
            ('ada.gnathub.db', db_path),
            ('ada.gnathub.src_mapping', _escpath(SonarQube.src_mapping())),
            ('ada.file.suffixes', ','.join(suffixes)),
            ('scm.exclusions.disabled', 'true')
        ])

        # Set project properties
        self._set_project_customizable_dict(
            SonarScannerProperties._generate_customizable_attr(project_name))
        self._set_dict(non_customizable_attributes)

    def _generate_multi_module(self, db_path, project_name, suffixes):
        """Generate part of the :file:`sonar-project.properties` configuration.

        Do not create the file yet. See :meth:`write` for this.

        :param str db_path: full path to the DB
        :param str project_name: project name
        :param collections.Iterable[str] suffixes: list of Ada extensions
        """
        modules = {k: v for k, v in
                   list(GNAThub.Project.source_dirs().items()) if v}
        _, modules = self._generate_source_dirs(modules)

        non_customizable_attributes = collections.OrderedDict([
            ('language', 'ada'),
            ('ada.gnathub.db', db_path),
            ('ada.gnathub.src_mapping', _escpath(SonarQube.src_mapping())),
            ('ada.file.suffixes', ','.join(suffixes)),
            ('scm.exclusions.disabled', 'true'),
            ('modules', ','.join([m.lower() for m in list(modules.keys())]))
        ])

        # Set project properties
        self._set_project_customizable_dict(
            SonarScannerProperties._generate_customizable_attr(project_name))
        self._set_dict(non_customizable_attributes)

        project_key = self._get('projectKey')

        # Set modules properties
        for subproject_name, sources in list(modules.items()):
            module_attributes = collections.OrderedDict([
                ('projectName', subproject_name),
                ('projectKey', '%s::%s' % (project_key, subproject_name)),
                ('projectBaseDir', _escpath(SonarQube.src_cache())),
                ('sources', sources)
            ])

            self._set_dict(module_attributes, module=subproject_name.lower())

    def _generate_source_dirs(self, modules):
        """Generate the source directories configuration.

        Copy over all sources in a temporary directory before running the Sonar
        Scanner. This is to work around recent versions of SonarQube source
        importer implementation that looks recursively in source directories
        (which is inconsistent with GPR files semantic).

        :param modules: project modules and their associated source directories
        :type modules: dict[str,list[str]]
        :return: the path to the root source directory and a copy of the input
            ``modules`` directory with updated path to source directories
            (pointing to the local copy)
        :rtype: (str, dict[str,list[str]])
        """
        self.log.debug('caching source dirs prior to sonar-scanner execution')

        # Compute the total dirs count to copy to display progress
        count = 0
        total = sum([len(dirs) for dirs in modules.values()])

        root_src_dir = SonarQube.src_cache()

        self.info('prepare source dirs for sonar-scanner')
        self.log.info(
             'copy source files from the project closure to %s',
             root_src_dir)

        # Remove any previous analysis left-over
        shutil.rmtree(root_src_dir, ignore_errors=True)
        new_modules_mapping = collections.OrderedDict()

        for module_name in modules:
            module_root_src_dir = os.path.join(root_src_dir, module_name)
            new_modules_mapping[module_name] = _escpath(module_root_src_dir)

            # Add an additional subdirectory.
            # NOTE: SonarQube uses "[root]" as the root directory name, which
            # means that when we have a flat list of source files, they all end
            # up in different "[root]" dirs in SonarQube UI.  Since each
            # project can have a "[root]" directory, it makes it harder to read
            # the hierarchy.  To work around that, we interpose an additional
            # "<module-name>-src" directory to make things clearer from a UI
            # point of view.
            module_root_src_dir = os.path.join(module_root_src_dir,
                                               module_name.lower() + '-src')

            # Create the local source directory
            if not os.path.exists(module_root_src_dir):
                os.makedirs(module_root_src_dir)

            # Compute the base directory for source dirs.
            # NOTE: The GNAT Project language allows the user to specify a list
            # of source directories that are not necessarily all under the same
            # hierarchy. Furthermore, and this is the default behavior, it
            # allows the user to specify non-recursive source directories:
            # source directories which contain sub-directories which are not
            # themselves source directories. The SonarQube model requires all
            # sources in specified source directories to be in the source
            # closure: to satisfy this requirement, we gather all sources under
            # a same tree, and do a best effort to mimic the original
            # organization of sources.
            module_src_dirs = modules[module_name]
            dirs_commonprefix = os.path.commonprefix(module_src_dirs)
            self.log.info('source dirs common prefix: %s', dirs_commonprefix)

            # Use a dict to ensure we don't have duplicated names
            src_files = {}

            # Copy each source dir content
            self.info('prepare files from module: %s' % module_name)
            for src_dir in modules[module_name]:
                src_dir_relpath = os.path.relpath(src_dir, dirs_commonprefix)
                src_dir_path = os.path.join(module_root_src_dir,
                                            src_dir_relpath)
                self.log.info(' + %s' % src_dir_path)

                if not os.path.exists(src_dir_path):
                    os.makedirs(src_dir_path)

                # Copy over all files
                for entry in os.listdir(src_dir):
                    entry_path = os.path.join(src_dir, entry)

                    if not os.path.isfile(entry_path):
                        continue

                    if entry in src_files:
                        self.error('duplicated source file: %s' % entry)
                        self.error('  + %s' % src_files[entry])
                        self.error('  + %s' % entry_path)

                    src_files[entry] = entry_path

                    new_path = os.path.join(src_dir_path, entry)
                    self.log.debug('%s -> %s', entry_path, new_path)

                    shutil.copy(entry_path, new_path)
                    self.src_mapping[_escpath(entry_path)] = \
                        _escpath(os.path.normpath(new_path))

                count = count + 1
                Console.progress(count, total, count == total)

        return root_src_dir, new_modules_mapping

    def write(self, properties_fname=None):
        """Dump the SonarQube Scanner configuration files.

        Creates the following configuration file:

            * :file:`sonar-project.properties` -> ``properties_fname``
            * :file:`sources-mapping.properties`
                    -> :meth:`SonarQube.src_mapping()`

        :param properties_fname: the configuration file name (if ``None``, use
            :meth:`SonarQube.configuration()`)
        :type properties_fname: str or None
        """
        def _escape(key):
            """Escape ``key``.

            Escapes the given key to comply with java.util.Properties parser
            (see the ``java.util.Properties.load`` method documentation).

            :param str key: the key to escape
            :return: the escaped key
            :rtype: str
            :see: docs.oracle.com/javase/8/docs/api/java/util/Properties.html
            """
            escaped = key
            for sym in (':', '=', ' '):
                escaped = escaped.replace(sym, r'\%s' % sym)
            return escaped

        if not properties_fname:
            properties_fname = SonarQube.configuration()

        self.info('generate %s' % properties_fname)
        with open(properties_fname, 'w') as configuration:
            for pair in list(self.attributes.items()):
                configuration.write('%s = %s\n' % pair)

        self.info('generate %s' % SonarQube.src_mapping())
        with open(SonarQube.src_mapping(), 'w') as mapping:
            for key, value in list(self.src_mapping.items()):
                mapping.write('%s = %s\n' % (_escape(key), value))
