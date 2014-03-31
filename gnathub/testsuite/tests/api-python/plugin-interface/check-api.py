"""Check the integrity of the GNAThub Python module."""

# pylint: disable=invalid-name

import GNAThub


# Global variable supposedly updated by MyCustomPlugin
MY_VARIABLE = False


# pylint: disable=no-init, too-few-public-methods
class MyIncompletePlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface.

    However, do not implement the EXECUTE method to test error case.
    """

    name = 'My Incomplete Plugin'


# pylint: disable=no-init, too-few-public-methods
class MyCustomPlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface."""

    name = 'My Custom Plugin'

    # pylint: disable=no-self-use, global-statement
    def execute(self):
        """Overridden."""
        global MY_VARIABLE
        MY_VARIABLE = True


# GNAThub.Plugin interface not implemented
try:
    MyIncompletePlugin()
    assert False, 'MyIncompletePlugin is not expected to be instantiable'
except TypeError:
    # A type error occurs when the EXECUTE method is not overridden
    pass


PLUGIN = MyCustomPlugin()

# GNAThub.Plugin.name
assert PLUGIN.name == 'My Custom Plugin', \
    'unexpected plugin name "%s"' % PLUGIN.name

# GNAThub.Plugin.exec_status (getter)
assert PLUGIN.exec_status == GNAThub.NOT_EXECUTED, \
    'unexpected plugin execution status "%s"' % repr(PLUGIN.exec_status)

# GNAThub.Plugin.exec_status (setter)
try:
    PLUGIN.exec_status = 'invalid value'
    assert False, 'exec_status affectation should have raised an exception'
except GNAThub.Error:
    pass

PLUGIN.exec_status = GNAThub.EXEC_SUCCESS
