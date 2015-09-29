"""Provide mock objects for the GNAThub testsuite."""

import os

from gnatpython.ex import Run, PIPE, STDOUT
from gnatpython.fileutils import cp, sync_tree

from support import const


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
        """Run :command:`make target` and return the process.

        :param str target: The Makefile target to call.
        :returns: gnatpython.ex.Run
        """

        make = Run(['make', target], cwd=self.install_dir, output=PIPE,
                   error=STDOUT)
        return make

    def build(self):
        """Build the Ada project using the Makefile `build` rule.

        :raises: AssertionError
        """

        process = self.make('build')
        assert not process.status, '%s: unexpected build failure' % self.name

    def run(self):
        """Run the executable using the Makefile `run` rule.

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
        """Run GNAThub with the appropriate command line.

        :param bool quiet: The --quiet switch.
        :param bool verbose: The --verbose switch.
        :param list[str] plugins: The --plugins PLUGINS switch.
        """

        argv = ['gnathub', '-P', self.project.name]

        if kwargs.get('quiet', True):
            argv.append('--quiet')

        if kwargs.get('verbose', False):
            argv.append('--verbose')

        if kwargs.get('scenario_vars', None):
            scenario = kwargs['scenario_vars']
            assert isinstance(scenario, dict), 'invalid "scenario_vars" arg'
            argv.extend(['-X%s=%s' % (k, v) for k, v in scenario.iteritems()])

        if kwargs.get('plugins', None):
            assert isinstance(kwargs['plugins'], list), 'invalid "plugin" arg'
            argv.extend(['--plugins', ','.join(kwargs['plugins'])])

        if kwargs.get('target', None):
            argv.extend(['--target', kwargs['target']])

        if kwargs.get('runtime', None):
            argv.extend(['--RTS', kwargs['runtime']])

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
