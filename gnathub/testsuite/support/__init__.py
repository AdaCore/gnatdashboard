"""Support module for the GNAThub testsuite."""

import os
from support import const


# The directory containing the support Python module
const.basedir = os.path.dirname(os.path.realpath(__file__))

# The directory containing the test cases resources
const.resourcedir = os.path.join(const.basedir, 'resources')

# The directory containing the GNAThub scripts
const.scriptdir = os.path.join(const.resourcedir, 'scripts')
