# GNAThub (GNATdashboard)
# Copyright (C) 2016-2017, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

"""GNAThub plug-in for the generation of a standalone rich HTML report.

It exports the HTMLReport class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import GNAThub

import inspect
import os

from GNAThub import Console, Plugin, Reporter

from shutil import copy2, copytree, rmtree
from _report import ReportBuilder


class HTMLReport(Plugin, Reporter):
    """HTMLReport plugin for GNAThub."""

    @property
    def name(self):
        return 'html-report'

    @property
    def webapp_dir(self):
        """Return the path to the webapp directory.

        This directory contains the generic parts of the web application.

        :rtype: str
        """
        this = inspect.getfile(inspect.currentframe())
        return os.path.join(os.path.dirname(os.path.dirname(this)), 'webui')

    @property
    def output_dir(self):
        """Return the path to the directory where to generate the HTML report.

        :return: the full path to the output directory
        :rtype: str
        """

        return os.path.join(GNAThub.root(), self.name)

    def verbose_info(self, message):
        if (GNAThub.verbose()):
            self.info(message)

    def report(self):
        """Generate JSON-encoded representation of the data collected."""

        # The output directory for the JSON-encoded report data
        data_output_dir = os.path.join(self.output_dir, 'data')
        data_src_output_dir = os.path.join(data_output_dir, 'src')

        try:
            self.info('generate JSON-encoded report')

            # Create directory structure if needed
            if not os.path.exists(self.output_dir):
                os.makedirs(self.output_dir)
            else:
                self.log.warn('%s: already exists', self.output_dir)
                self.log.warn('existing report may be overriden')

            # Copy the generic web application files
            for entry in os.listdir(self.webapp_dir):
                path = os.path.join(self.webapp_dir, entry)
                dest = os.path.join(self.output_dir, entry)
                if os.path.isdir(path):
                    self.log.debug('rm -r "%s"', dest)
                    if os.path.isdir(dest):
                        rmtree(dest)
                    self.log.debug('cp -r "%s" "%s"', path, dest)
                    copytree(path, dest)
                else:
                    self.log.debug('cp "%s" "%s"', path, dest)
                    copy2(path, dest)

            # Create the JSON-encoded report output directory
            for directory in (data_output_dir, data_src_output_dir):
                if not os.path.exists(directory):
                    os.makedirs(directory)

            # The report builder initially starts empty. The more sources
            # processed, the more complete the report.
            report = ReportBuilder()

            # Generate the JSON-representation of each source of the project.
            for count, source in enumerate(report.iter_sources(), start=1):
                dest = '{}.json'.format(
                    os.path.join(data_src_output_dir, source.filename))
                source.save_as(dest)
                self.log.debug('%s: saved as %s', source.filename, dest)
                Console.progress(count, report.index.source_file_count, False)

            # Generate the JSON-encoded report for message navigation.
            dest = os.path.join(data_output_dir, 'message.json')
            report.index.message_to_json(dest)
            self.log.debug('message index saved as %s', dest)
            self.verbose_info('HTML report message generated in ' + dest)

            # Generate the JSON-encoded report for filter panel.
            dest = os.path.join(data_output_dir, 'filter.json')
            report.index.filter_to_json(dest)
            self.log.debug('filter index saved as %s', dest)
            self.verbose_info('HTML report filter generated in ' + dest)

            # Generate the JSON-encoded report for code navigation.
            dest = os.path.join(data_output_dir, 'code.json')
            report.index.code_to_json(dest)
            self.log.debug('code index saved as %s', dest)
            self.verbose_info('HTML report code generated in ' + dest)

            # Generate the JSON-encoded report for custom review status.
            dest = os.path.join(data_output_dir, 'custom_status.json')
            report.index.custom_review_to_json(dest)
            self.log.debug('custom review status saved as %s', dest)
            self.verbose_info('HTML report custom review status generated in '
                              + dest)

            codepeer_obj_dir = os.path.join(GNAThub.Project.object_dir(),
                                            'codepeer')
            if os.path.isdir(codepeer_obj_dir):
                # Call to codepeer_bridge for offline mode
                self.log.debug("Export info from codepeer_bridge")
                dest = os.path.join(GNAThub.Project.object_dir(),
                                    'gnathub', 'html-report',
                                    'data', 'codepeer_review.xml')
                name = 'codepeer_bridge'
                cmd = ['codepeer_bridge',
                       '--output-dir=' + GNAThub.output_dir(),
                       '--db-dir=' + GNAThub.db_dir(),
                       '--export-reviews=' + dest]
                self.log.debug('Codepeer_bridge file generated in %s', dest)
                self.verbose_info('Codepeer_bridge file generated in ' + dest)
                GNAThub.Run(name, cmd)

                # Get codepeer_run file
                copy_file = os.path.join(GNAThub.Project.object_dir(),
                                         'codepeer',
                                         'codepeer_run')
                dest = os.path.join(GNAThub.Project.object_dir(),
                                    'gnathub', 'html-report',
                                    'data')
                copy2(copy_file, dest)
                self.log.debug('Codepeer_run file copied in %s', dest)
                self.verbose_info('Codepeer_run file copied in ' + dest)

                # Get race_condition file
                copy_file = os.path.join(GNAThub.Project.object_dir(),
                                         'codepeer',
                                         GNAThub.Project.name().lower()
                                         + '.output',
                                         'race_conditions.xml')
                if os.path.isfile(copy_file):
                    dest = os.path.join(GNAThub.Project.object_dir(),
                                        'gnathub', 'html-report',
                                        'data')
                    copy2(copy_file, dest)
                    self.log.debug('%s file copied in %s', copy_file, dest)
                    self.verbose_info(copy_file + ' file copied in ' + dest)

        except Exception as why:
            self.log.exception('failed to generate the HTML report')
            self.error(str(why))
            return GNAThub.EXEC_FAILURE
        except IOError as why:
            self.log.exception('failed to generate the HTML report')
            self.error(str(why))
            return GNAThub.EXEC_FAILURE

        else:
            return GNAThub.EXEC_SUCCESS
