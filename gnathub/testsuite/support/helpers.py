"""Helper functions for the testsuite."""


def uniq(array):
    """Remove all duplicates from `array`

    :type array: list[*]
    :param array: the array to uniquify
    :return: a new array without duplicates
    :rtype: list[*]
    """
    return list(set(array))
