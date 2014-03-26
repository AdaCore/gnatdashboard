.. include:: ../defines.hrst

Writing a |Driver| plug-in
==========================

Location for user-defined plug-ins
----------------------------------

Store your plug-ins in the :file:`extra` directory. |Driver| attempts to
load all files in this directory except the ones whose name starts with an
underscore (:kbd`_`), which are expected to be support files referenced by
multiple plug-ins.


Plug-in structure
-----------------

A |Driver| plug-in is a Python class that extends the
:class:`GNAThub.Plugin` abstract class. It must override the
:meth:`GNAThub.Plugin.execute` method and set the :command:`TOOL_NAME` and
:command:`LOG_FILE` attributes.

Additionally, the user can override the two following methods:

* :meth:`GNAThub.Plugin.setup`
* :meth:`GNAThub.Plugin.teardown`

These will be called respectively before and after the
:meth:`GNAThub.Plugin.execute` method.

Plug-in execution
-----------------

The plug-in is discovered and loaded by the |Driver| driver unless
explicitly disabled in the project file using the :command:`Plugins_Off`
attribute.  If it remains enabled, it is executed along with the other
plugins without any further action.

