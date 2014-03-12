.. include:: ../defines.hrst

Writing a |Driver| plug-in
==========================

User-defined's plug-ins location
--------------------------------

User's plug-ins should be stored in the **extra** directory. |Driver| attemps to
load all files in this directory except the ones which name starts with an
underscore (``_``). These files are expected to be support files (*eg.* holding
code factorized between two or more plugins).


Plug-in structure
-----------------

A |Driver| plug-in is a Python class that extends the ``GNAThub.Plugin``
abstract class. It should at the very least override the
``GNAThub.Plugin.execute`` method and set the ``TOOL_NAME`` and ``LOG_FILE``
attributes.

Additionally, the user can override the two following methods:

    * ``GNAThub.Plugin.setup()``
    * ``GNAThub.Plugin.teardown()``

These will be called respectively before and after the ``execute`` method.


Plug-in execution
-----------------

The plug-in is discovered and loaded by the |Driver| driver. Unless explicitly
disabled in the project file *via* the ``Plugins_Off`` attribute, it will be
executed along the other plugins without any further action from the user.
