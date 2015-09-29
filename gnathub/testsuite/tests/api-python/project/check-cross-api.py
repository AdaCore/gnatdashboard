"""Check the integrity of the GNAThub Python module."""

import GNAThub

# GNAThub.Project.target (not overridden)
assert GNAThub.Project.target() == 'arm-eabi', \
    'unexpected project target: "%s"' % GNAThub.Project.target()

# GNAThub.Project.runtime (not overridden)
assert GNAThub.Project.runtime() == 'ravenscar-sfp-stm32f4', \
    'unexpected project runtime "%s"' % GNAThub.Project.runtime()
