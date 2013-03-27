#! /usr/bin/env python2.7

## GPS Qualimetrics module ####################################################
##


## File Overview ##############################################################
##
## This file is a GPS script that is intended to be loaded using the --load
## command line switch of GPS, e.g.:
##
##    $ gps --hide -Proot.gpr --load=python:qmt.py
##
## The purpose of this GPS module is to parse the root project file and
## retrieve all sources referenced in the project tree (i.e. by the root
## project file and its dependencies).
##


## Imports ####################################################################
##

import os
import traceback
import sys

import GPS


## ProjectManager #############################################################
##

class ProjectManager(object):
  """Implements the flyweight pattern to handle only one instance of each
  project during the script execution time.
  """

  def __init__(self):
    """Initializes the Project Manager."""
    self.__projects = {}


  def get(self, gps_project):
    """Returns a unique reference to the Project class associated with the
    given GPS.Project.
    """

    if gps_project.file().name() not in self.__projects.keys():
      self.__projects[gps_project.file().name()] = Project(gps_project)

    return self.__projects[gps_project.file().name()]


## Project ####################################################################
##

class Project(object):
  """References a project as delcared in the associated project file. The class
  is given the project object as input to its contructor, and recursively build
  the project tree from it.

  This class provides multiple helpers to manipulate the project tree.
  """

  _manager = ProjectManager()

  def __init__(self, gps_project):
    """Initializes the class."""
    self.__gps_project = gps_project
    self.__dependencies = [self._manager.get(p)
                           for p in self.__gps_project.dependencies()]


  def name(self):
    """Returns the project's name."""
    return self.__gps_project.name()


  def path(self):
    """Returns the project file path on the filesystem as passed to the class
    constructor.
    """
    return self.__gps_project.file().name()


  def object_dir(self):
    """returns the object dir path for this project file.
       returns None if the project file has no object directory.
    """

    # GPS.Project.object_dirs function takes an optional parameter:
    #
    #     Return the list of object directories for this project. If Recursive
    #     is True, the source directories of imported projects is also
    #     returned. There might be duplicate directories in the returned list
    #
    #       :param recursive: A boolean
    #       :return: A list of strings

    # In the case of a non-recursive call, GPS.Project.object_dirs returns
    #  a empty list.
    object_dirs = self.__gps_project.object_dirs(False)
    if object_dirs:
        return object_dirs[0]
    return None


  def sources(self):
    """Returns a list of GPS.File object for each source related to the
    project.
    """
    return self.__gps_project.sources()


  def get_flat_dependency_list(self, dependencies=[]):
    """Returns a flat list of all dependencies for this project.
    The list returned is duplicate-free.
    """

    for project in self.__dependencies:
      # The following works because of the ProjectManager handling unique
      # referencies to each project.
      if project not in dependencies:
        dependencies.append(project)
        dependencies = project.get_flat_dependency_list(dependencies)

    return dependencies


  def __repr__ (self):
    """Returns the project representation string following this format:

        {
          "Object_Dir": "path/to/object/dir",
          "Source_Dirs": {
            "Source_Dir_1": ["source.ads", "source.adb"],
            "Source_Dir_2": ["foobar.ads", "foobar.adb"]
          }
        }
    """

    source_dirs = {}

    for source_dir in self.__gps_project.source_dirs():
      source_dirs[source_dir[:-1]] = []

    for source in self.__gps_project.sources():
      (source_dir, filename) = os.path.split(source.name())
      assert source_dir in source_dirs.keys()
      source_dirs[source_dir].append(filename)

    project_repr = {}
    object_dir = self.object_dir()
    if object_dir:
        project_repr['Object_Dir'] = object_dir
    project_repr['Source_Dirs'] = source_dirs

    return repr(project_repr)


## RootProject ################################################################
##

class RootProject(Project):

  def __init__(self):
    """Initializes the class. Calls the super class constructor with the root
    project object as argument.
    """
    super(RootProject, self).__init__(GPS.Project.root())


  def dump_source_list(self, stream):
    """Dumps the source list to STREAM.
    The output is in JSON format:

        {
          "Project_1": {
            "Object_Dir": "path/to/object/dir",
            "Source_Dirs": {
              "Source_Dir_1": ["source.ads", "source.adb"],
              "Source_Dir_2": ["foobar.ads", "foobar.adb"]
          },
          "Project_2": {
            ...
          }
        }
    """

    projects = [self]
    projects.extend(self.get_flat_dependency_list())

    output = {}

    for project in projects:
      output[project.name()] = project

    stream.write('%s%s' % (repr(output).replace("'", '"'), os.linesep))


## Helpers ####################################################################
##

def __init_module():
  """Initializes the module.

  For now, it simply restores sys.stdout and sys.stderr to their initial value
  since the GPS module overrides them.
  """

  sys.stdout = sys.__stdout__
  sys.stderr = sys.__stderr__


## Module Entry Point #########################################################
##

def __qmt_entry_point(hook):
  """Entry point for the script. This function is hooked to the 'gps_started'
  event of the GPS API and thus is spawned when GPS starts.

  It loads the project tree and dumps the list of sources for the whole
  project (i.e. including its dependencies).
  """

  try:
    # Set up the script environment.
    __init_module()

    # Load the root project passed to GPS through the -P command line option.
    project = RootProject()

    # Dump the source list on stdout.
    project.dump_source_list(sys.stdout)

  except Exception as e:
    sys.stderr.write('Exception raised: %s%s' % (str(e), os.linesep))

    # Print the stack trace.
    exc_type, exc_value, exc_traceback = sys.exc_info()
    traceback.print_exception(exc_type, exc_value, exc_traceback,
                              file=sys.stderr)

  finally:
    GPS.exit()


## Hook Instruction ###########################################################
##

# Hook the module entry point into GPS.
GPS.Hook('gps_started').add(__qmt_entry_point)
