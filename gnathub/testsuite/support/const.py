"""Module that will be initialized with constants."""

import sys


# pylint: disable=too-few-public-methods
class _const(object):
    """Handles the constness of this module's attributes."""

    class ConstError(TypeError):
        """Raised when trying to set the value of an existing attribute."""
        pass

    def __setattr__(self, name, value):
        if name in self.__dict__:
            raise self.ConstError("Can't rebind const (%s)" % name)

        self.__dict__[name] = value

sys.modules[__name__] = _const()
