#! /usr/bin/env python2.7

## Qualimetrics module ########################################################
##


## File Overview ##############################################################
##
## This file is a Python script that is intended to parse the result of the GPS
## python module qmt.py (which outputs JSON-like data) and provides helper
## functions to deal with this data.
##
## Example of use:
##
##   * Is the given project loaded?
##       $ gps -Proot.gpr --load=python:qmt.py | qmt_parser.py -P AWS
##       True
##
##   * Is the given source file exists and if so return its full path.
##       $ gps -Proot.gpr --load=python:qmt.py | qmt_parser.py -f aws.ads
##       /full/path/to/aws.ads
##       $ echo $?
##       0
##


## Imports ####################################################################
##

import argparse
import json
import os
import sys


## __parse_command_line #######################################################
##

def __parse_command_line():
  """Initializes and parses the command line, and returns the result of that
  parsing operation, i.e. an object with and opened file descriptor to the
  JSON input, and both PROJECT and FILE attributes initialized accordingly with
  the command line.
  """

  parser = argparse.ArgumentParser(description='Parse the output of qmt.py')
  parser.add_argument('-i', '--input', nargs='?', type=argparse.FileType('r'),
                      default='-',
                      help=('The input JSON file. If not specified, it is read',
                            ' from the standard input'))

  group = parser.add_mutually_exclusive_group()
  group.add_argument('-P', '--project', type=str,
                     help='Whether the given project is used or not')
  group.add_argument('-f', '--file', type=str,
                     help='Return the fullpath for the given source file')
  return parser.parse_args()


## __has_project ##############################################################
##

def __has_project(json_data, project):
  """Displays True or False on the standard output depending on whether the
  given project is either the root project or a dependency project as specified
  in the JSON input.

  This function always returns 0.
  """

  sys.stdout.write('%r' % (project in json_data.keys()))
  sys.stdout.write(os.linesep)
  return 0


## __get_fullpath #############################################################
##

def __get_fullpath(json_data, basename):
  """Searches for the source that has BASENAME as basename, and prints its
  full path as specified in the JSON input.

  Returns 0 on success (the corresponding full path was found), 1 otherwise.
  """

  for project, source_dirs in json_data.items():
    for source_dir, sources in source_dirs.items():
      if basename in sources:
        sys.stdout.write(source_dir)
        sys.stdout.write(os.linesep)
        return 0

  return 1


## __entry_point ##############################################################
##

def __entry_point():
  """Parses the command line and execute the correct helper function depending
  on the user input.

  Quits silently if no action is requested by the user (neither -P nor -f
  specified on the command line) with a value of 1 as exit code.
  """

  cmdline = __parse_command_line()
  json_data = json.load(cmdline.input)
  assert isinstance(json_data, dict)

  if cmdline.project is not None:
    return __has_project(json_data, cmdline.project)

  if cmdline.file is not None:
    return __get_fullpath(json_data, cmdline.file)

  return 1


## Script Entry Point #########################################################
##

if __name__ == '__main__':
  """Calls the entry point and uses its return value as exit code."""
  sys.exit(__entry_point())
