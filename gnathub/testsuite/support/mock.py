"""Provide mock objects for the GNAThub testsuite."""

import json
import os

from gnatpython.env import BaseEnv, Env
from gnatpython.ex import Run, PIPE, STDOUT
from gnatpython.fileutils import cp, chmod, mkdir, sync_tree

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

        return Run(
            ['make', target], cwd=self.install_dir, output=PIPE, error=STDOUT)

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
    def gcov_multi_object_dir(dst=os.getcwd()):
        return Project('gcov_multi_object_dir', dst)

    @staticmethod
    def disabled(dst=os.getcwd()):
        return Project('disabled', dst)


class MockedExecutable(object):

    """Mocked executable."""

    def __init__(self, name, project, ecode=0):
        self.name = name
        self.ecode = ecode
        self.project = project

    def _generate_code(self):
        return '#! /bin/bash\n\nexit %d' % self.ecode

    def create(self, output_dir):
        mkdir(output_dir)
        executable = os.path.join(output_dir, self.name)
        if Env().build.os.name == 'windows':
            # On Windows, suffixes executables by .exe
            executable += '.exe'
        with open(executable, 'w') as fd:
            fd.write(self._generate_code() % {
                'project_name': self.project.name,
                'project_root': self.project.install_dir
            })
            fd.write('\n')
        chmod('+x', executable)


class CodePeerExecutable(MockedExecutable):

    """Mock the codepeer executable."""

    def __init__(self, *args, **kwargs):
        super(CodePeerExecutable, self).__init__('codepeer', *args, **kwargs)

    def _generate_code(self):
        return '''#! /bin/bash

mkdir -p obj/codepeer

while test $# -gt 0
do
  case "$1" in
    -output-msg|-output-msg-only)
      exec cat %(project_root)s/codepeer.injected-results.csv
      ;;
  esac
  shift
done

exit 0
'''


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


class GNAThubExecutionFailed(Exception):
    pass


class GNAThub(object):
    def __init__(self, project, mocks=None, **kwargs):
        """Run GNAThub on the given project.

        :param project: the project on which to run GNAThub
        :type project: Project
        :param mocks: optional list of executables to mock and the expected
            exit code they should return
        :type mocks: list[MockedExecutable]
        """
        self.project = project
        self._bin = os.path.join(self.project.install_dir, 'bin')
        for mock in mocks or []:
            self.mock_executable(mock(self.project))
        self.run(**kwargs)

    def mock_executable(self, mock):
        """Create the mock script.

        :param mock: the mock class
        :type mock: MockedExecutable
        """
        mock.create(self._bin)

    def run(self, **kwargs):
        """Run GNAThub with the appropriate command line.

        :param kwargs: Keyword arguments that translate into command line
            switches
        :type kwargs: dict[str,*] | None
        """

        argv = ['gnathub', '-P', self.project.name]

        if kwargs.get('quiet', True):
            argv.append('--quiet')

        if kwargs.get('verbose', False):
            argv.append('--verbose')

        if kwargs.get('dry_run', False):
            argv.append('--dry-run')

        if kwargs.get('runners_only', False):
            argv.append('--runners-only')

        if kwargs.get('reporters_only', False):
            argv.append('--reporters-only')

        if kwargs.get('scenario_vars', None):
            scenario = kwargs['scenario_vars']
            assert isinstance(scenario, dict), 'invalid "scenario_vars" arg'
            argv.extend(['-X%s=%s' % (k, v) for k, v in scenario.iteritems()])

        if kwargs.get('plugins', None):
            assert isinstance(kwargs['plugins'], list), 'invalid "plugins" arg'
            argv.extend(['--plugins', ','.join(kwargs['plugins'])])

        if kwargs.get('subdirs', None):
            argv.extend(['--subdirs', kwargs['subdirs']])

        if kwargs.get('target', None):
            argv.extend(['--target', kwargs['target']])

        if kwargs.get('runtime', None):
            argv.extend(['--RTS', kwargs['runtime']])

        for tool_name, arguments in kwargs.get('tool_args', {}).iteritems():
            argv.append('--targs:%s' % tool_name)
            argv.extend(arguments)
            argv.append('--')

        if kwargs.get('script', None):
            argv.append('--exec')

            if isinstance(kwargs['script'], str):
                argv.append(os.path.abspath(kwargs['script']))

            elif isinstance(kwargs['script'], Script):
                argv.append(kwargs['script'].path)

            else:
                raise TypeError('expected str or support.Script')

        # Add the local bin directory to the path to use mocks if any
        BaseEnv().add_path(self._bin)

        run_kwargs = {
            'output': kwargs.get('output', PIPE),
            'error': kwargs.get('error', STDOUT),
            'timeout': kwargs.get('timeout', None),
            'env': kwargs.get('env', None)
        }

        p = Run(argv, cwd=self.project.install_dir, **run_kwargs)
        if p.status != 0:
            raise GNAThubExecutionFailed(p.out)
        if kwargs.get('dry_run', False):
            # In dry-run mode, the gnathub.backlog file is not created
            return
        backlog = os.path.join(
            self.project.install_dir, 'obj', 'gnathub', 'gnathub.backlog')
        with open(backlog, 'r') as fd:
            plugins = json.loads(fd.read())
        failed = [name for name, results in plugins if not results['success']]
        if failed:
            raise GNAThubExecutionFailed(
                'plugin(s) failure: {}: {}'.format(failed, p.out))
