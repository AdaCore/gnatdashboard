#! /usr/bin/env python

"""
Setup configuration file for GNAThub Python library.
"""

import os
from distutils.core import setup


ROOT_DIR = os.path.dirname(__file__)

if ROOT_DIR != '':
    os.chdir(ROOT_DIR)

# Run the setup tools
setup(
    name='GNAThub',
    version=open(os.path.join('..', '..', 'VERSION')).read(),
    author='AdaCore',
    author_email='report@adacore.com',
    license='GPLv3',
    url='https://www.adacore.com',
    description='A Python framework for building GNAThub plug-ins.',
    packages=['GNAThub'],
    scripts=[os.path.join('scripts', 'plugin-runner.py')],
)
