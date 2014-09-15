############################################################################
#                                                                          #
#                               G N A T h u b                              #
#                                                                          #
#                     Copyright (C) 2013-2014, AdaCore                     #
#                                                                          #
# This is free software;  you can redistribute it  and/or modify it  under #
# terms of the  GNU General Public License as published  by the Free Soft- #
# ware  Foundation;  either version 3,  or (at your option) any later ver- #
# sion.  This software is distributed in the hope  that it will be useful, #
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License for  more details.  You should have  received  a copy of the GNU #
# General  Public  License  distributed  with  this  software;   see  file #
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy #
# of the license.                                                          #
#                                                                          #
############################################################################

"""Provides common components to GNAT Pro Tool Suite.
"""

# A source location regex pattern
SLOC_PATTERN = \
    '(?P<file>[a-zA-Z-_.0-9]+):(?P<line>[0-9]+)(:(?P<column>[0-9]+))?'

SLOC_NO_TAG_PATTERN = '[a-zA-Z-_.0-9]+:[0-9]+(:[0-9]+)?'
