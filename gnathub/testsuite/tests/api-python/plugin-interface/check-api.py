"""Check the integrity of the GNAThub Python module."""

import os
import sys
sys.path.append(os.environ["GNATHUB_COREDIR"])
import GNAThub

from support.asserts import assertEqual, assertRaises


# Global variables supposedly updated by MyCustomPlugin
MY_RUN_VARIABLE = False
MY_REPORT_VARIABLE = False


class MyIncompleteRunnerPlugin(GNAThub.Plugin, GNAThub.Runner):
    """Declare a custom plugin that extends the GNAThub Plugin interface.

    However, do not implement the RUN method to test error case.
    """

    name = 'My Incomplete Plugin'


class MyIncompleteReporterPlugin(GNAThub.Plugin, GNAThub.Reporter):
    """Declare a custom plugin that extends the GNAThub Plugin interface.

    However, do not implement the REPORT method to test error case.
    """

    name = 'My Incomplete Plugin'


class MyCustomReporterPlugin(GNAThub.Plugin, GNAThub.Reporter):
    """Declare a custom plugin that extends the GNAThub reporter interface."""

    name = 'My Custom Plugin'

    def report(self):
        """Overridden."""
        pass


class MyCustomRunnerPlugin(GNAThub.Plugin, GNAThub.Runner):
    """Declare a custom plugin that extends the GNAThub runner interface."""

    name = 'My Custom Plugin'

    def run(self):
        """Overridden."""
        pass


class MyCustomPlugin(GNAThub.Plugin, GNAThub.Runner, GNAThub.Reporter):
    """Declare a custom plugin that extends the GNAThub Plugin interface."""

    name = 'My Custom Plugin'

    def run(self):
        """Overridden."""
        global MY_RUN_VARIABLE
        MY_RUN_VARIABLE = True

    def report(self):
        """Overridden."""
        global MY_REPORT_VARIABLE
        MY_REPORT_VARIABLE = True


# GNAThub.Runner interface not implemented
with assertRaises(TypeError):
    # A type error occurs when the RUN method is not overridden
    MyIncompleteRunnerPlugin()

# GNAThub.Reporter interface not implemented
with assertRaises(TypeError):
    # A type error occurs when the REPORT method is not overridden
    MyIncompleteReporterPlugin()

MyCustomRunnerPlugin()    # Fails if it raises an exception
MyCustomReporterPlugin()  # Fails if it raises an exception
PLUGIN = MyCustomPlugin()

# GNAThub.Plugin.name
assertEqual(PLUGIN.name, 'My Custom Plugin')

# GNAThub.Plugin.exec_status (getter)
assertEqual(PLUGIN.exec_status, GNAThub.NOT_EXECUTED)

# GNAThub.Plugin.exec_status (setter)
with assertRaises(GNAThub.Error):
    PLUGIN.exec_status = 'invalid value'

PLUGIN.exec_status = GNAThub.EXEC_SUCCESS
