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

"""???"""

import GNAThub

from GNAThub import GPSTarget, Log


class OutputParserMetaClass(type):
    """Meta Class for GPS Target output parsers."""

    registered = dict()   # list of registered parsers

    def __new__(mcs, name, bases, attrs):
        new_class = type.__new__(mcs, name, bases, attrs)
        OutputParserMetaClass.registered[new_class.get_name()] = new_class

        return new_class

    def get_name(mcs):
        """Returns the name of the parser, either from a "name" class
        attribute, or from the class name.

        RETURNS
            :rtype: a string
        """

        return getattr(mcs, 'name', mcs.__name__).lower()


class OutputParser(object):
    """A default implementation of an output parser."""

    __metaclass__ = OutputParserMetaClass

    def __init__(self, child):
        """Instance constructor."""
        self.child = child

    def on_stdout(self, text):
        """Function called by the GPSTarget engine upon data to write on the
        standard output stream.

        PARAMETERS
            :param text: the text to output
            :type text: a string
        """

        if self.child is not None:
            self.child.on_stdout(text)

    def on_stderr(self, text):
        """Function called by the GPSTarget engine upon data to write on the
        standard error stream.

        PARAMETERS
            :param text: the text to output
            :type text: a string
        """

        if self.child is not None:
            self.child.on_stderr(text)

    def on_exit(self, status=0):
        """Function called by the GPSTarget engine upon inferior exit.

        PARAMETERS
            :param status: the inferior exit status
            :type text: a number
        """

        Log.debug('Process exit status: %s' % status)

        # Update Tool execution status
        if status != 0:
            GPSTarget.EXECUTION_SUCCESS = GNAThub.EXEC_FAIL
        else:
            GPSTarget.EXECUTION_SUCCESS = GNAThub.EXEC_SUCCESS

        if self.child is not None:
            self.child.on_exit(status)


def create_parser(name, child=None):
    """Factory function that creates an output parser.

    PARAMETERS
        :param name: the name of the registerd output parser
        :type name: a string
        :param child: ???
        :type child: ???
    """

    if name in OutputParserMetaClass.registered:
        return OutputParserMetaClass.registered[name](child)

    return None
