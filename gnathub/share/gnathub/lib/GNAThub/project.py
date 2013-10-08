##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                        Copyright (C) 2013, AdaCore                       ##
##                                                                          ##
## The QM is free software; you can redistribute it  and/or modify it       ##
## under terms of the GNU General Public License as published by the Free   ##
## Software Foundation; either version 3, or (at your option) any later     ##
## version.  The QM is distributed in the hope that it will be useful,      ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License  for more details. You  should  have  received a copy of the GNU ##
## General Public License  distributed with the QM; see file COPYING3. If   ##
## not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite ##
## 330, Boston, MA 02111-1307, USA.                                         ##
##                                                                          ##
##############################################################################

"""Helper functions for accessing project attributes and properties."""

# pylint: disable=F0401
# Disable "Unable to import" error
import GPS


PACKAGE = 'GNAThub'


def root():
    """Returns the root project.

    RETURNS
        :rtype: a GPS.Project
    """

    return GPS.Project.root()


def object_dir():
    """Returns the project object directory path.

    RETURNS
        :rtype: a string
    """

    return root().object_dirs()[0]


def name():
    """Returns the project name.

    RETURNS
        :rtype: a string
    """

    return root().name()


def path():
    """Returns the path to the project file.

    RETURNS
        :rtype: a string
    """

    return root().file().name()


def property_as_string(key):
    """Returns project property from package GNAThub as a string.

    RETURNS
        :rtype: a string
    """

    return root().get_attribute_as_string(attribute=key, package=PACKAGE)


def property_as_list(key):
    """Returns project property from package GNAThub as a list.

    RETURNS
        :rtype: a string
    """

    return root().get_attribute_as_list(attribute=key, package=PACKAGE)
