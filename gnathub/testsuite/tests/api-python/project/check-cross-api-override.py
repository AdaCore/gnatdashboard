"""Check the integrity of the GNAThub Python module."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub
from support.asserts import assertEqual

# GNAThub.Project.target (overridden by switches)
assertEqual(GNAThub.Project.target(), 'my-custom-target')

# GNAThub.Project.runtime (overridden by switches)
assertEqual(GNAThub.Project.runtime(), 'my-custom-runtime')
