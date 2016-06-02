"""Assert methods."""


def assertEqual(first, second):
    """Test that `first` and `second` are equal.

    If the values do not compare equal, the test will fail.
    """
    if first != second:
        raise AssertionError('%s != %s' % (first, second))


def assertNotEqual(first, second):
    """Test that `first` and `second` are not equal.

    If the values do compare equal, the test will fail.
    """
    if first == second:
        raise AssertionError('%s == %s' % (first, second))


def assertTrue(expr):
    """Test that `expr` is true.

    Note that this is equivalent to `bool(expr) is True` and not to
    `expr is True` (use `assertIs(expr, True)` for the latter). This method
    should also be avoided when more specific methods are available (e.g.
    `assertEqual(a, b)` instead of `assertTrue(a == b)`), because they provide
    a better error message in case of failure.
    """
    if bool(expr) is False:
        raise AssertionError('%s is not True' % expr)


def assertFalse(expr):
    """Test that `expr` is false.

    Note that this is equivalent to `bool(expr) is False` and not to
    `expr is False` (use `assertIs(expr, False)` for the latter). This method
    should also be avoided when more specific methods are available (e.g.
    `assertEqual(a, b)` instead of `assertTrue(a != b)`), because they provide
    a better error message in case of failure.
    """
    if bool(expr) is True:
        raise AssertionError('%s is not False' % expr)


def assertEmpty(container):
    """Test that `container` is empty."""
    if len(container) != 0:
        raise AssertionError('%s is not empty' % (container,))


def assertNotEmpty(container):
    """Test that `container` is not empty."""
    if len(container) == 0:
        raise AssertionError('%s is empty' % (container,))


def assertIs(first, second):
    """Test that `first` and `second` evaluate to the same object."""
    if first is not second:
        raise AssertionError('%s is not %s' % (first, second))


def assertIsNot(first, second):
    """Test that `first` and `second` don't evaluate to the same object."""
    if first is second:
        raise AssertionError('%s is %s' % (first, second))


def assertIsNone(expr):
    """Test that `expr` is None."""
    if expr is not None:
        raise AssertionError('%s is not None' % expr)


def assertIsNotNone(expr):
    """Test that `expr` is not None."""
    if expr is None:
        raise AssertionError('%s is None' % expr)


def assertIn(first, second):
    """Test that `first` is in `second`."""
    if first not in second:
        raise AssertionError('%s is not in %s' % (first, second))


def assertNotIn(first, second):
    """Test that `first` is not in `second`."""
    if first in second:
        raise AssertionError('%s is in %s' % (first, second))


def assertIsInstance(obj, cls):
    """Test that `obj` is an instance of `cls` (which can be a class or a tuple
    of classes, as supported by isinstance()).

    To check for the exact type, use `assertIs(type(obj), cls)`.
    """
    if not isinstance(obj, cls):
        raise AssertionError('%s is not an instance of %s' % (obj, cls))


def assertNotIsInstance(obj, cls):
    """Test that `obj` is not an instance of `cls` (which can be a class or a
    tuple of classes, as supported by isinstance()).
    """
    if isinstance(obj, cls):
        raise AssertionError('%s is an instance of %s' % (obj, cls))


class _AssertRaisesContext(object):

    """A context manager used to implement TestCase.assertRaises() method."""

    def __init__(self, expected):
        self.expected = expected

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, tb):
        if exc_type is None:
            try:
                exc_name = self.expected.__name__
            except AttributeError:
                exc_name = str(self.expected)
            raise AssertionError('{} not raised'.format(exc_name))
        return issubclass(exc_type, self.expected)


def assertRaises(exception):
    """Test that an exception is raised within the context manager."""
    return _AssertRaisesContext(exception)
