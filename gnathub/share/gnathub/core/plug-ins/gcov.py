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

import os
import re
import GPS
import GNAThub

from GNAThub import GPSTarget, Log
from GNAThub import utils
from GNAThub import Session, dao, db
from GNAThub.db import Rule, Message, LineMessage
from GNAThub.utils import OutputParser, create_parser

## Gcov #######################################################################
##
class Gcov(GNAThub.Plugin):
    """Gcov plugin for GNAThub

       Retreieve .gcov generated files from project root object directory
       and feeds the DB with retrieved data from those files.
    """
    LOG_FILE_NAME='gcov.log'
    GCOV_SUFFIX='.gcov'

    def __init__ (self, session):
        super(Gcov, self).__init__('Gcov')
        self.session = session
        self.tool = dao.save_tool(self.session, self.name)
        self.rule = Rule('coverage', 'coverage',
                         self.tool, db.METRIC_KIND)

    def __add_hits_for_line(self, resource, line_num, hits):
        line = dao.get_or_create_line_from_resource_id (self.session,
                                                        resource.id,
                                                        line_num)

        if line:
            line_message = LineMessage()
            line_message.message = Message(hits, self.rule)
            line.messages.append(line_message)
        #else:
        #    Log.warn('Skipping coverage information for source: %s, because file not found' \
        #                % src_basename)

    def parse_gcov_output(self):
        # Fetch all files in project object directory and retrieve only
        # .gcov files, absolute path
        files = [os.path.join(utils.get_project_obj_dir(), f)
                for f in os.listdir(utils.get_project_obj_dir())
                if f.endswith(self.GCOV_SUFFIX)]

        # If no .gcov file found, plugin returns on failure
        if len(files) == 0:
            Log.fatal('No gcov file found in project root object directory')
            return GNAThub.EXEC_FAIL

        try:
            for f in files:
                # Retrieve source basename:
                # /path/to/source.adb.gcov --> source.adb
                src = f.split(os.sep)[-1:][0] \
                       .split(self.GCOV_SUFFIX)[0]
                resource = dao.get_file(self.session, src)

                if resource:
                    with open(f, 'r') as gcov_file:
                        # Retrieve information for every source line
                        # skip first 2 lines
                        for line in gcov_file.readlines()[2:]:

                            # Skip useless line
                            if line.strip()[0] != '-':
                                line_infos = line.split(':', 2)
                                hits = line_infos[0].strip()
                                # Line is not covered
                                if hits == '#####' or hits == '=====':
                                    hits = '0'
                                line_id = line_infos[1].strip()
                                self.__add_hits_for_line(resource, line_id, hits)

            self.session.commit()
            return GNAThub.EXEC_SUCCESS

        except IOError as e:
            Log.warn(str(e))
            return GNAThub.EXEC_FAIL

    def execute(self):
        return self.parse_gcov_output()

#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
#print (output)

