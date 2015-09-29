"""Check the integrity of the GNAThub Python module."""

import GNAThub

# GNAThub.Project.target (overridden by switches)
assert GNAThub.Project.target() == 'my-custom-target', \
    '%s: unexpected project target' % GNAThub.Project.target()

# GNAThub.Project.runtime (overridden by switches)
assert GNAThub.Project.runtime() == 'my-custom-runtime', \
    '%s: unexpected project runtime' % GNAThub.Project.runtime()
