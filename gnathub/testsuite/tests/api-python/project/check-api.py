"""Check the integrity of the GNAThub Python module."""

# pylint: disable=invalid-name

import GNAThub

import os
import sys

# The base directory for PATH comparisons
BASEDIR = os.path.dirname(os.path.realpath(sys.argv[0]))


def relpath(path):
    """Returns the relative path to :param:`path` from BASEDIR.

    :param str path: The full path.
    :returns: str

    """

    return os.path.relpath(path, BASEDIR)


# GNAThub.Project.name
assert GNAThub.Project.name() == 'Disabled', \
    '%s: unexpected project name' % GNAThub.Project.name()

# GNAThub.Project.path
assert relpath(GNAThub.Project.path()) == 'disabled.gpr', \
    '%s: unexpected project path' % GNAThub.Project.path()

# GNAThub.Project.object_dir
obj = GNAThub.Project.object_dir()
assert relpath(obj) == 'obj', '%s: unexpected project path' % obj
assert os.path.isdir(obj), '%s: no such directory' % obj

source_dirs = GNAThub.Project.source_dirs()
assert len(source_dirs) == 1, \
    'unexpected number of projects: expected 1, got %d' % len(source_dirs)
assert 'Disabled' in source_dirs, 'missing project "Disabled"'
assert relpath(source_dirs['Disabled'][0]) == 'src', \
    '$s: unexpected source dirs, expected "src"' % source_dirs['Disabled'][0]

# GNAThub.Project.source_file
source = GNAThub.Project.source_file('simple.adb')
expected = 'src%ssimple.adb' % os.path.sep
assert relpath(source) == expected, '%s: unexpected source file' % source
assert os.path.isfile(source), '%s: no such file' % source

# GNAThub.Project.property_as_string
project_name = GNAThub.Project.property_as_string('Project_Name')
assert project_name == 'My_Disabled_Project', \
    '%s: unexpected property value for Project_Name' % project_name

project_key = GNAThub.Project.property_as_string('Project_Key')
assert project_key == 'Disabled :: Core', \
    '%s: unexpected property value for Project_Key' % project_key

project_version = GNAThub.Project.property_as_string('Project_Version')
assert project_version == '1.2.1b', \
    '%s: unexpected property value for Project_Version' % project_version

encoding = GNAThub.Project.property_as_string('Source_Encoding')
assert encoding == 'My_Custom_Encoding', \
    '%s: unexpected property value for Encoding' % project_version

# GNAThub.Project.property_as_list
plugins = GNAThub.Project.property_as_list('Plugins')
assert 'sonar-config' in plugins, 'missing "sonar-config" plugin in Plugins'

local_repo = GNAThub.Project.property_as_string('Local_Repository')
assert relpath(local_repo) == 'local_repo', \
    '$s: unexpected local repository, expected "local_repo"' % local_repo

main = GNAThub.Project.property_as_string('Main', package='')
assert main == 'simple.adb'

plugins_off = GNAThub.Project.property_as_list('Plugins_Off')
assert 'gnatcheck' in plugins_off, 'missing "gnatcheck" plugin in Plugins_Off'
assert 'gnatmetric' in plugins_off, \
    'missing "gnatmetric" plugin in Plugins_Off'
assert 'gcov' in plugins_off, 'missing "gcov" plugin in Plugins_Off'
assert 'codepeer' in plugins_off, 'missing "codepeer" plugin in Plugins_Off'
assert 'gnatprove' in plugins_off, 'missing "gnatprove" plugin in Plugins_Off'
