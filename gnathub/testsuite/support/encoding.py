"""Test categories encoding."""

import os
import yaml

from gnatpython.fileutils import unixpath
from support import const


# Handle encoding of tests name
class TestEncoder(object):

    """Provides mecanism to encode, decode a test name."""

    encoding = None
    reverse_encoding = None

    @classmethod
    def _load_config(cls):
        """Loads the configuration file (containing the mappings directories /
        encoding) if necessary.
        """

        if cls.encoding is not None and cls.reverse_encoding is not None:
            return

        with open(os.path.join(const.basedir, 'category-encoding.yaml')) as fd:
            cls.encoding = yaml.load(fd.read())
            cls.reverse_encoding = {v: k for k, v in cls.encoding.items()}

    @classmethod
    def encode(cls, path):
        """Encode a test name.

        Use the dirname to compute the testcase display name. This uses
        mappings set in the configuration file. This file is loaded if
        necessary.
        """

        cls._load_config()

        path, basename = os.path.split(unixpath(path))
        return '%s.%s' % (cls.encoding['/'.join(path.split('/'))], basename)

    @classmethod
    def decode(cls, name):
        """Decode a test name.

        Use the dirname to compute the testcase full path. This uses
        mappings set in the configuration file. This file is loaded if
        necessary.
        """

        cls._load_config()

        encoded, basename = name.rsplit('.', 1)
        return '%s/%s' % (cls.reverse_encoding[encoded], basename)
