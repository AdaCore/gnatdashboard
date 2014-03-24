"""Check the integrity of the GNAThub Python module."""

# pylint: disable=C0103
# Disable "Invalid module name" (this is a script, not a module)

import GNAThub

import os
import sys

# The base directory for PATH comparisons
BASEDIR = os.path.dirname(os.path.realpath(sys.argv[0]))


def relpath(path):
    """Relative path to PATH from BASEDIR."""
    return os.path.relpath(path, BASEDIR)


# GNAThub.Project.name
assert GNAThub.Project.name() == 'default', \
    '%s: unexpected project name' % GNAThub.Project.name()

# GNAThub.Project.path
assert relpath(GNAThub.Project.path()) == 'default.gpr', \
    '%s: unexpected project path' % GNAThub.Project.path()

# GNAThub.Project.object_dir
obj = GNAThub.Project.object_dir()
assert relpath(obj) == 'obj', '%s: unexpected project path' % obj
assert os.path.isdir(obj), '%s: no such directory' % obj

source_dirs = GNAThub.Project.source_dirs()
assert len(source_dirs) == 2, \
    'unexpected number of Source Dirs: expected 2, got %d' % len(source_dirs)
assert relpath(source_dirs[0]) == '.', \
    '$s: unexpected source dirs, expected "."' % source_dirs[0]
assert relpath(source_dirs[1]) == 'src', \
    '$s: unexpected source dirs, expected "src"' % source_dirs[1]

# GNAThub.Project.source_file
source = GNAThub.Project.source_file('hello.adb')
assert relpath(source) == 'hello.adb', '%s: unexpected source file' % source
assert os.path.isfile(source), '%s: no such file' % source

# GNAThub.Project.property_as_string
project_key = GNAThub.Project.property_as_string('Project_Key')
assert project_key == 'Default::Main', \
    '%s: unexpected property value for Project_Key' % project_key

project_version = GNAThub.Project.property_as_string('Project_Version')
assert project_version == '1.0.0b', \
    '%s: unexpected property value for Project_Version' % project_version

encoding = GNAThub.Project.property_as_string('Source_Encoding')
assert encoding == 'My_Encoding', \
    '%s: unexpected property value for Encoding' % project_version

# GNAThub.Project.property_as_list
plugins = GNAThub.Project.property_as_list('Plugins')
assert 'codepeer' in plugins, 'missing "codepeer" plugin in Plugins'
assert 'sonarconfig' in plugins, 'missing "sonarconfig" plugin in Plugins'

specific = GNAThub.Project.property_as_list('Specific_Plugins')
assert 'foo' in specific, 'missing "foo" plugin in Specific_Plugins'
assert 'bar' in specific, 'missing "bar" plugin in Specific_Plugins'

plugins_off = GNAThub.Project.property_as_list('Plugins_Off')
assert 'codepeer' in plugins_off, 'missing "codepeer" plugin in Plugins_Off'
assert 'gnatprove' in plugins_off, 'missing "gnatprove" plugin in Plugins_Off'
