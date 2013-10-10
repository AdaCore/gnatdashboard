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

"""Provides common components to GNAT Pro Tool Suite.
"""

import GNAThub
import re

from GNAThub import Log


class GNATToolProgressProtocol(GNAThub.LoggerProcessProtocol):
    """A custom LoggerProcessProtocol to additionally provide the user with
    command-line output feedback about the execution progress.
    """

    REMAINING = re.compile('^Units remaining: (?P<count>[0-9]+)')

    def __init__(self, tool):
        """Instance constructor."""

        GNAThub.LoggerProcessProtocol.__init__(self, tool)
        self.total = None

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def errReceived(self, data):
        """Inherited."""

        GNAThub.LoggerProcessProtocol.errReceived(self, data)

        match = self.REMAINING.match(data)

        if match:
            count = int(match.group('count'))

            if self.total is None:
                self.total = count
                Log.progress(1, self.total)
            else:
                Log.progress(self.total - count, self.total)

    # pylint: disable=C0103
    # Disable "Invalid Name" error
    def processEnded(self, reason):
        """Inherited."""

        GNAThub.LoggerProcessProtocol.processEnded(self, reason)

        if self.total:
            Log.progress(self.total, self.total, new_line=True)

        self.plugin.postprocess(self.exit_code)

        # Ensure that we don't break the plugin chain.
        self.plugin.ensure_chain_reaction()
