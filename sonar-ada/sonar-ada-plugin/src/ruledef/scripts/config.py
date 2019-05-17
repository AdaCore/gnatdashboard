# Sonar Ada Plugin (GNATdashboard)
# Copyright (C) 2019, AdaCore
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

"""Collection of utilities for integration with the SonarQube platform."""

# Predefined remediation effort constants based on SonarQube docs
# (From https://docs.sonarqube.org/display/DEV/Coding+Rule+Guidelines)

TRIVIAL_REMEDIATION_EFFORT = "5min"
EASY_REMEDIATION_EFFORT = "10min"
MEDIUM_REMEDIATION_EFFORT = "20min"
MAJOR_REMEDIATION_EFFORT = "60min"
HIGH_REMEDIATION_EFFORT = "180min"
COMPLEX_REMEDIATION_EFFORT = "1440min"
