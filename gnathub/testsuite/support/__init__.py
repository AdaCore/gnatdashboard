"""Support module for the GNAThub testsuite."""

import os
import yaml

from gnatpython.ex import Run, PIPE, STDOUT
from gnatpython.fileutils import cp, sync_tree, unixpath

from support import const


# The directory containing the support Python module
const.basedir = os.path.dirname(os.path.realpath(__file__))

# The directory containing the test cases resources
const.resourcedir = os.path.join(const.basedir, 'resources')

# The directory containing the GNAThub scripts
const.scriptdir = os.path.join(const.resourcedir, 'scripts')


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
            cls.reverse_encoding = {v: k for k, v in cls.encoding.iteritems()}

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


class Project(object):
    """Project resource accessor."""

    def __init__(self, name, dest):
        self.name = name
        self.dest = dest
        self.path = os.path.join(const.resourcedir, 'testcase-' + self.name)

        assert os.path.isdir(self.path), 'unknown resource'

        self.install_dir = os.path.join(self.dest, self.name)
        self.deploy(self.install_dir)

    def make(self, target):
        """Runs :command:`make target` and returns the resulting process
        object.

        :param str target: The Makefile target to call.
        :returns: gnatpython.ex.Run

        """

        make = Run(['make', target], cwd=self.install_dir, output=PIPE,
                   error=STDOUT)
        return make

    def build(self):
        """Builds the Ada project using the Makefile "build" rule that all
        resource sets must implement.

        :raises: AssertionError

        """

        process = self.make('build')
        assert not process.status, '%s: unexpected build failure' % self.name

    def run(self):
        """Runs the executable using the Makefile "run" rule that all resource
        sets must implement.

        This Makefile rule is expected to generate coverage information.

        :raises: AssertionError

        """

        process = self.make('run')
        assert not process.status, '%s: unexpected build failure' % self.name

    def deploy(self, dest):
        sync_tree(self.path, dest, delete=True)

    @staticmethod
    def simple(dst=os.getcwd()):
        return Project('simple', dst)

    @staticmethod
    def coverage_exhaustive(dst=os.getcwd()):
        return Project('coverage_exhaustive', dst)

    @staticmethod
    def disabled(dst=os.getcwd()):
        return Project('disabled', dst)


class Script(object):
    """Script resource accessor."""

    def __init__(self, name, dest):
        self.name = name
        self.dest = dest
        self._path = os.path.join(const.scriptdir, self.name + '.py')

        assert os.path.isfile(self._path), 'unknown script'

        self.deploy(self.dest)

    @property
    def path(self):
        return os.path.join(self.dest, self.name + '.py')

    def deploy(self, dest):
        cp(self._path, dest)

    @staticmethod
    def db2cfg(dst=os.getcwd()):
        return Script('db2cfg', dst)


class GNAThub(object):
    def __init__(self, project, **kwargs):
        self.project = project
        self.run(**kwargs)

    def run(self, **kwargs):
        """Runs GNAThub with the appropriate command line.

        :param bool quiet: The --quiet switch.
        :param bool verbose: The --verbose switch.
        :param list[str] plugins: The --plugins PLUGINS switch.

        """

        argv = ['gnathub', '-P', self.project.name]

        if kwargs.get('quiet', True):
            argv.append('--quiet')

        if kwargs.get('verbose', False):
            argv.append('--verbose')

        if kwargs.get('external_refs', None):
            ext_refs = kwargs['external_refs']
            assert isinstance(ext_refs, dict), 'invalid "external_refs" arg'
            argv.extend(['-X%s=%s' % (k, v) for k, v in ext_refs.iteritems()])

        if kwargs.get('plugins', None):
            assert isinstance(kwargs['plugins'], list), 'invalid "plugin" arg'
            argv.extend(['--plugins', ','.join(kwargs['plugins'])])

        if kwargs.get('script', None):
            argv.append('--exec')

            if isinstance(kwargs['script'], str):
                argv.append(os.path.abspath(kwargs['script']))

            elif isinstance(kwargs['script'], Script):
                argv.append(kwargs['script'].path)

            else:
                raise TypeError('expected str or support.Script')

        run_kwargs = {
            'output': kwargs.get('output', PIPE),
            'error': kwargs.get('error', STDOUT),
            'timeout': kwargs.get('timeout', None),
            'env': kwargs.get('env', None)
        }

        p = Run(argv, cwd=self.project.install_dir, **run_kwargs)
        assert not p.status, 'gnathub execution failed: %s' % p.out
