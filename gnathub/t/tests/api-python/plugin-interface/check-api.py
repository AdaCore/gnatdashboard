"""Check the integrity of the GNAThub Python module."""

# pylint: disable=C0103
# Disable "Invalid module name" (this is a script, not a module)

import GNAThub


# Global variable supposedly updated by MyCustomPlugin
MY_VARIABLE = False


# pylint: disable=no-init, too-few-public-methods
class MyIncompletePlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface.

    However, do not implement the EXECUTE method to test error case.
    """

    TOOL_NAME = 'My Incomplete Plugin'
    LOG_FILE = 'incomplete.log'


# pylint: disable=no-init, too-few-public-methods
class MyCustomPlugin(GNAThub.Plugin):
    """Declare a custom plugin that extends the GNAThub Plugin interface."""

    TOOL_NAME = 'My Custom Plugin'
    LOG_FILE = 'custom.log'

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

# GNAThub.Plugin.display_command_line()
assert PLUGIN.display_command_line() == 'my custom plugin', \
    ('unexpected plugin display command line "%s"' %
     PLUGIN.display_command_line())

# GNAThub.Plugin.fqn
assert PLUGIN.fqn == 'gnathub.my-custom-plugin', \
    'unexpected plugin fully qualified name "%s"' % PLUGIN.fqn

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

# Register the plugin and run the GNAThub execution
GNAThub.register(PLUGIN)

# Ensure that the plugin did run
assert not MY_VARIABLE, 'control assay'
GNAThub.run()
assert MY_VARIABLE, 'MyCustomPlugin.execute not executed'
