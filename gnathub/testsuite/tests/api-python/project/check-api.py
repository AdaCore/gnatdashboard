"""Check the integrity of the GNAThub Python module."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

import os.path

from support.asserts import assertEqual, assertIn, assertTrue
from support.helpers import uniq

# The base directory for PATH comparisons
BASEDIR = os.path.dirname(os.path.realpath(sys.argv[0]))


def relpath(path):
    """Return the relative path to :param:`path` from BASEDIR.

    :param str path: The full path.
    :returns: str
    """

    return os.path.relpath(path, BASEDIR)


# GNAThub.Project.name
assertEqual(GNAThub.Project.name(), 'Disabled')

# GNAThub.Project.path
assertEqual(relpath(GNAThub.Project.path()), 'disabled.gpr')

# GNAThub.Project.object_dir
assertEqual(relpath(GNAThub.Project.object_dir()), 'obj')
assertTrue(os.path.isdir(GNAThub.Project.object_dir()))

# GNAThub.Project.source_dirs
source_dirs = GNAThub.Project.source_dirs()
assertEqual(len(source_dirs), 1)
assertIn('Disabled', source_dirs)
assertEqual(relpath(source_dirs['Disabled'][0]), 'src')

# GNAThub.Project.source_files
source_files = GNAThub.Project.source_files()
assertEqual(len(source_files), 1)
assertIn('Disabled', source_files)
assertEqual(
    sorted([os.path.basename(sfile) for sfile in source_files['Disabled']]),
    sorted(['f.adb', 'f.ads', 'simple.adb'])
)

# Check consistency between GNAThub.Project.source_dirs() and
# GNAThub.Project.source_files().
assertEqual(
    sorted(uniq([
        os.path.normpath(os.path.dirname(sfile))
        for sfile in source_files['Disabled']
    ])),
    sorted([os.path.normpath(dir) for dir in source_dirs['Disabled']])
)

# GNAThub.Project.source_file
filename = GNAThub.Project.source_file('simple.adb')
expected = 'src%ssimple.adb' % os.path.sep
assertEqual(relpath(filename), expected)
assertTrue(os.path.isfile(filename))

# GNAThub.Project.property_as_string
project_name = GNAThub.Project.property_as_string('Project_Name')
assertEqual(project_name, 'My_Disabled_Project')

project_key = GNAThub.Project.property_as_string('Project_Key')
assertEqual(project_key, 'Disabled :: Core')

project_version = GNAThub.Project.property_as_string('Project_Version')
assertEqual(project_version, '1.2.1b')

encoding = GNAThub.Project.property_as_string('Source_Encoding')
assertEqual(encoding, 'My_Custom_Encoding')

# GNAThub.Project.property_as_list
plugins = GNAThub.Project.property_as_list('Plugins')
assertIn('sonar-config', plugins)

local_repo = GNAThub.Project.property_as_string('Local_Repository')
assertEqual(relpath(local_repo), 'local_repo')

main = GNAThub.Project.property_as_string('Main', package='')
assertEqual(main, 'simple.adb')

exec_dir = GNAThub.Project.property_as_string('Exec_Dir', package='')
assertEqual(exec_dir, 'src')

plugins_off = GNAThub.Project.property_as_list('Plugins_Off')
assertIn('gnatcheck', plugins_off)
assertIn('gnatmetric', plugins_off)
assertIn('gcov', plugins_off)
assertIn('codepeer', plugins_off)

# GNAThub.Project.scenaria_switches
scenario_vars = GNAThub.Project.scenario_switches()
expected_vars = (
    '-XBUILD_MODE=Production',
    '-XVERSION=test-0.0.0',
    '-XBUILD_DIR=/some/user/workspace/project/build/dir',
    '-XPROCESSORS=2'
)
assertEqual(len(scenario_vars), len(expected_vars))
for var in expected_vars:
    assertIn(var, scenario_vars)
