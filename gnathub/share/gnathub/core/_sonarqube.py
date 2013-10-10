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

import GNAThub

import os


class SonarQube(object):

    EXEC_DIRECTORY = 'sonar'
    CONFIGURATION = 'sonar-project.properties'

    @staticmethod
    def workdir():
        return os.path.join(GNAThub.root(), SonarQube.EXEC_DIRECTORY)

    @staticmethod
    def configuration():
        return os.path.join(SonarQube.workdir(), SonarQube.CONFIGURATION)

    @staticmethod
    def make_workdir():
        if not os.path.exists(SonarQube.workdir()):
            os.makedirs(SonarQube.workdir())
