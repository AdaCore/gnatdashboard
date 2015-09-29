#! /usr/bin/env python

"""Run the testsuite.

This module implements the GNATpython testsuite driver."""

import argparse
import os
import pygments
import sys

from collections import defaultdict

from gnatpython.env import Env
from gnatpython.fileutils import echo_to_file, find, split_file
from gnatpython.main import Main
from gnatpython.mainloop import MainLoop
from gnatpython.mainloop import add_mainloop_options
from gnatpython.mainloop import generate_run_testcase
from gnatpython.mainloop import setup_result_dir
from gnatpython.reports import ReportDiff
from gnatpython.testdriver import add_run_test_options

from pygments.formatters import get_formatter_by_name
from pygments.lexers.text import DiffLexer
from pygments.style import Style
from pygments.styles import get_style_by_name

from pygments.token import Token as Token

from support import const
from support.encoding import TestEncoder


# Directory from where this script is invoked
ORIGIN = os.getcwd()

# Base directory for the testsuite
BASEDIR = os.path.dirname(os.path.realpath(__file__))


def update_dict(origin, extras):
    """
    Update the content of ORIGIN with the content of EXTRAS, and return the
    former.

    PARAMETERS
        origin: the dictionary to update
        extras: the dictionary to update ORIGIN with

    RETURNS
        an updated copy of ORIGIN
    """

    final = origin.copy()
    final.update(extras)
    return final


class GNAThubOutputStyle(Style):
    """The pygments style to apply to the output."""

    default_style = ''
    native = get_style_by_name('native')
    styles = update_dict(native.styles, {
        Token.TestResult.OK: native.styles[Token.Generic.Inserted],
        Token.TestResult.DIFF: native.styles[Token.Generic.Error],
        Token.TestResult.CRASH: native.styles[Token.Generic.Error],
        Token.TestResult.XFAIL: native.styles[Token.Comment],
        Token.TestResult.UOK: native.styles[Token.Number],

        Token.Punctuation: native.styles[Token.Comment]
    })


class Testsuite(object):
    """The GNAThub testsuite."""

    # Status that report an error
    ERRORS = ('DIFF', 'CRASH')

    def __init__(self):
        self.duration = 0
        self.summary = defaultdict(lambda: 0)

        self.formatter = None
        self.testcases = None

        self.testcase_runner = None

        self.env = Env()
        self.discs = [self.env.target.platform]

        self.main = Main()
        add_mainloop_options(self.main, extended_options=True)
        add_run_test_options(self.main)

        self.main.add_option('--with-diff', action='store_true', default=False,
                             help='show diffs on stdout')

        self.main.add_option('--colorize', action='store_true',
                             default=False, help=argparse.SUPPRESS)

    @staticmethod
    def find_testcases(directory):
        """Find all testcases in the given directory."""

        return set(sorted(find(directory, pattern='test.py') +
                          find(directory, pattern='test.sh')))

    @staticmethod
    def compute_testcases_list(args):
        """Return the list of testcases to execute.

        PARAMETERS
            args: the testsuite positional command-line arguments

        RETURNS
            the list of testcases
        """

        tests = None

        if args:
            tests = [os.path.relpath(os.path.join(ORIGIN, test), BASEDIR)
                     for test in args]
        else:
            basedir = os.path.join(BASEDIR, 'tests')
            tests = [os.path.relpath(os.path.dirname(p), BASEDIR)
                     for p in Testsuite.find_testcases(basedir)]

        return [TestEncoder.encode(path) for path in tests]

    def parse_command_line(self):
        """Handle command-line parsing and internal configuration."""

        self.main.parse_args()

        self.formatter = get_formatter_by_name(
            'terminal256' if self.main.options.colorize else 'null',
            style=GNAThubOutputStyle, encoding='utf-8')

        self.testcases = Testsuite.compute_testcases_list(self.main.args)
        self.testcases = sorted(self.testcases, key=lambda s: s.lower())

        if self.main.options.discs:
            self.discs.extend(self.main.options.discs.split(','))

        setup_result_dir(self.main.options)

    def execute(self):
        """Run the testsuite and execute testcases."""

        # Add the support directory in the PYTHONPATH so that modules are
        # accessible from each test case.
        Env().add_search_path('PYTHONPATH', os.path.dirname(const.basedir))

        self.parse_command_line()

        self.testcase_runner = generate_run_testcase(
            os.path.join(BASEDIR, 'run-test'),
            self.discs, self.main.options)

        MainLoop(self.testcases,
                 self.testcase_runner,
                 self.collect_result,
                 self.main.options.mainloop_jobs)

        # Generate the report file
        diff = ReportDiff(
            self.main.options.output_dir,
            self.main.options.old_output_dir
        )
        diff.txt_image(self.main.options.report_file)
        self.log(self.format_testsuite_summary())

    def log(self, tokens, stream=sys.stdout):
        """Log the input token stream with the standard Python logging
        mecanism.

        PARAMETERS
            log_fn: the logging function to use
            tokens: the input tokens stream
        """

        assert self.formatter is not None, 'Internal error'
        print >> stream, pygments.format(tokens, self.formatter)

    def collect_result(self, name, process, job_info):
        """Custom collect_result function."""

        def resource(ext):
            """Returns the path to the testcase resource with the given ext."""
            return os.path.join(
                self.main.options.output_dir, '%s.%s' % (name, ext))

        # Fetch the testcase results
        raw_result = split_file(resource('result'), ignore_errors=True)

        if raw_result:
            status, message = raw_result[0].split(':', 1)
        else:
            status, message = 'CRASH', 'testsuite internal error (no results)'

        if os.path.isfile(resource('time')):
            with open(resource('time'), 'r') as timing:
                duration = float(timing.read())
        else:
            duration = 0

        result = {
            'name': name,
            'status': status,
            'message': message,
            'duration': duration
        }

        # Increment the status count
        self.summary[status] += 1
        self.duration += duration

        # Store the testcase result in the results file
        echo_to_file(self.main.options.results_file,
                     '%(name)s:%(status)s: %(message)s\n' % result,
                     append=True)

        # Display the testcase result
        self.log(
            Testsuite.format_testcase_result(result),
            sys.stderr if status in Testsuite.ERRORS else sys.stdout)

        # Display the testcase diff if requested
        diff_file = resource('diff')

        if (status in Testsuite.ERRORS and os.path.isfile(diff_file) and
                self.main.options.with_diff):

            with open(diff_file, 'r') as diff:
                self.log(Testsuite.format_testcase_diff(diff.read().strip()),
                         stream=sys.stderr)

    @staticmethod
    def status_token(status):
        """Return the token to use for the given test case result status.

        RETURNS
            a pygments Token
        """

        return getattr(Token.TestResult, status, Token.Error)

    @staticmethod
    def format_testcase_diff(diff):
        """Format a testcase output diff.

        PARAMETERS
            diff: the diff content

        RETURNS
            a list of pygments' Tokens
        """

        def new_line_token():
            """Generate a new line token."""
            return Token.Whitespace, '\n'

        def indent_token():
            """Generate an indentation space token."""
            return Token.Whitespace, ' ' * 4

        tokens = []
        new_line = True

        # Because of logging prefixes, skip the first line to avoid
        # misalignment.
        tokens.append(new_line_token())

        for ttype, value in pygments.lex(diff, DiffLexer()):
            for subval in value.split('\n'):
                if new_line:
                    tokens.append(indent_token())

                new_line = not subval

                if subval:
                    tokens.append((ttype, subval))
                else:
                    tokens.append(new_line_token())

        return tokens

    @staticmethod
    def format_testcase_result(testcase):
        """Format the result of a single testcase.

        TESTCASE is expected to be a dictionary with the following attributes
        set:

            * name
            * status
            * message

        RETURNS
            a list of pygments' Tokens
        """

        pad = max(0, 5 - len(testcase['status']))
        tokens = []

        if pad:
            tokens.append((Token.Whitespace, ' ' * pad))

        tokens.extend([
            (Testsuite.status_token(testcase['status']), testcase['status']),
            (Token.Whitespace, '  '),
            (Token.Text, testcase['name'])
        ])

        if testcase['message']:
            tokens.extend([
                (Token.Punctuation, ':'),
                (Token.Whitespace, ' '),
                (Token.Comment, testcase['message'])
            ])

        if testcase['duration']:
            tokens.extend([
                (Token.Whitespace, ' '),
                (Token.Punctuation, '('),
                (Token.Comment, '%.2fs' % testcase['duration']),
                (Token.Punctuation, ')')
            ])

        return tokens

    def format_testsuite_summary(self):
        """Format the testsuite's execution summary.

        RETURNS
            a list of pygments' Tokens
        """

        return [
            (Token.Whitespace, '\n'),

            (Token.Number, str(sum(n for n in self.summary.values()))),
            (Token.Whitespace, ' '),
            (Token.Text, 'testcases executed'),
            (Token.Whitespace, ' '),
            (Token.Punctuation, '('),
            (Token.Comment, 'duration: %.2fs' % self.duration),
            (Token.Punctuation, ')'),
            (Token.Whitespace, '\n'),

            (Token.TestResult.OK,
             str(self.summary['OK'] + self.summary['UOK'])),
            (Token.Whitespace, ' '),
            (Token.Text, 'completed'),
            (Token.Punctuation, ','),
            (Token.Whitespace, ' '),

            (Token.TestResult.DIFF,
             str(self.summary['DIFF'] + self.summary['CRASH'])),
            (Token.Whitespace, ' '),
            (Token.Text, 'failed'),
            (Token.Punctuation, ','),
            (Token.Whitespace, ' '),

            (Token.TestResult.XFAIL, str(self.summary['XFAIL'])),
            (Token.Whitespace, ' '),
            (Token.Text, 'expectedly failed')
        ]


if __name__ == '__main__':
    os.chdir(BASEDIR)
    Testsuite().execute()
