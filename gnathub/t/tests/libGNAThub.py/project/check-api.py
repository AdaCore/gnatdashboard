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

# GNAThub.Project.property_as_list
plugins_off = GNAThub.Project.property_as_list('Plugin_Off')
assert 'codepeer' in plugins_off, 'missing "codepeer" plugin in Plugin_Off'
assert 'gnatprove' in plugins_off, 'missing "gnatprove" plugin in Plugin_Off'
