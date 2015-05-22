.. include:: ../defines.hrst

Writing a |GNAThub| plug-in
===========================

Location for user-defined plug-ins
----------------------------------

Store your plug-ins in the :file:`extra` directory. |GNAThub| attempts to
load all files in this directory except the ones whose name starts with an
underscore (:kbd:`_`), which are expected to be support files referenced by
multiple plug-ins.

Structure
---------

A |GNAThub| plug-in is a Python class that extends the
:class:`GNAThub.Plugin` abstract class. It must override the
:meth:`GNAThub.Plugin.execute` method and set the :command:`name` property.

Additionally, the user can override the two following methods:

* :meth:`GNAThub.Plugin.setup`
* :meth:`GNAThub.Plugin.teardown`

These will be called respectively before and after the
:meth:`GNAThub.Plugin.execute` method.

Execution
---------

The plug-in is discovered and loaded by the |GNAThub| driver unless
explicitly disabled in the project file using the :command:`Plugins_Off`
attribute.  If it remains enabled, it is executed along with the other
plugins without any further action.

Logging
-------

Plug-ins can integrate with the logging mechanism provided by the |GNAThub| API
through the :attr:`log` property of the :class:`GNAThub.Plugin` class, *e.g.*:

.. code-block:: python

  self.log.debug('resource found at %s', resource)

Note that the |GNAThub| API provides its own `logging.Handler
<https://docs.python.org/2.7/library/logging.html#handler-objects>`_
implementation to integrate with the standard `Python logging facility
<https://docs.python.org/2/library/logging.html>`_ meaning that one can use the
:mod:`logging` Python module directly and automatically benefit from this
integration. This becomes particularly useful when importing thirdparty modules
that already rely on this logging facility (*e.g.* `Python requests
<http://docs.python-requests.org/en/latest/>`_).
