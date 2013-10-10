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

# pylint: disable=C0103
# Disable "Invalid name {}"

"""Defines GNAThub-specific project attributes and load them with GPS."""

# pylint: disable=F0401
# Disable "Unable to import" error
import GPS


PROJECT_SUPPORT_DEFINITION = """<?xml version="1.0"?>
    <Project_Support>

        <project_attribute
            name="Project_Name"
            label="Project name"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The name of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Plugins"
            label="Plugins"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            list="true"
            description="GNATdashboard plugins to execute in order">
            <string />
        </project_attribute>

        <project_attribute
            name="Project_Version"
            label="Project Version"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The version of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Project_Key"
            label="Project Version"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The key identifier of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Source_Encoding"
            label="Source Encoding"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="Encoding of the source files">
            <string />
        </project_attribute>

        <project_attribute
            name="Specific_Plugins"
            label="Project Specific Plugin"
            package="Dashboard"
            editor_page="GNATdashboard"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            list="true"
            description="Path to project specific plugin for GNATdashboard">
            <string />
        </project_attribute>

</Project_Support>
"""

GPS.parse_xml(PROJECT_SUPPORT_DEFINITION)
