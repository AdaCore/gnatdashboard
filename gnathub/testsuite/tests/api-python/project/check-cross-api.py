"""Check the integrity of the GNAThub Python module."""

import GNAThub
from support.asserts import assertEqual

# GNAThub.Project.target (not overridden)
assertEqual(GNAThub.Project.target(), 'arm-eabi')

# GNAThub.Project.runtime (not overridden)
assertEqual(GNAThub.Project.runtime(), 'ravenscar-sfp-stm32f4')
