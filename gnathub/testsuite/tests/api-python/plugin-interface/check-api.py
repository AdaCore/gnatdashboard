"""Check the integrity of the GNAThub Python module."""

import GNAThub
from support.asserts import assertEqual, assertRaises


# Global variable supposedly updated by MyCustomPlugin
MY_VARIABLE = False


class MyIncompletePlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface.

    However, do not implement the EXECUTE method to test error case.
    """

    name = 'My Incomplete Plugin'


class MyCustomPlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface."""

    name = 'My Custom Plugin'

    def execute(self):
        """Overridden."""
        global MY_VARIABLE
        MY_VARIABLE = True


# GNAThub.Plugin interface not implemented
with assertRaises(TypeError):
    # A type error occurs when the EXECUTE method is not overridden
    MyIncompletePlugin()

PLUGIN = MyCustomPlugin()

# GNAThub.Plugin.name
assertEqual(PLUGIN.name, 'My Custom Plugin')

# GNAThub.Plugin.exec_status (getter)
assertEqual(PLUGIN.exec_status, GNAThub.NOT_EXECUTED)

# GNAThub.Plugin.exec_status (setter)
with assertRaises(GNAThub.Error):
    PLUGIN.exec_status = 'invalid value'

PLUGIN.exec_status = GNAThub.EXEC_SUCCESS
