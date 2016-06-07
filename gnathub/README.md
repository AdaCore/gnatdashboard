GNAThub, GNATdashboard command line driver
==========================================

GNAThub
-------

GNAThub is a command-line tool that connects the dots between the GNAT Pro Tool
Suite and the GNATdashboard plugin for SonarQube. It collects and aggregates all
results from the various tools into a single place and stores those data into a
single SQLite file.

Building from sources
---------------------

To build GNAThub from sources, you need to have GNU make available as a
prerequisite.

You need to use a recent version of the GNAT compiler (e.g. GNAT 7.1.2).

You need a Python distribution (see `Install a Python distribution`).
You need an install of GNATcoll to build the project (see
`Fetching and building GNATcoll`)

Environment setup
'''''''''''''''''

Fetch GNAThub sources:

    $ cd $WORKSPACE
    $ git clone ssh://git.eu.adacore.com/gnatdashboard

Install a Python distribution
'''''''''''''''''''''''''''''

GNAThub runs on top of a Python distribution. You will need to download one from
the Python website and deploy it in the following directory:

    $WORKSPACE/gnatdashboard/gnathub/build/share/gnathub/python

The rest of this file refers to that Python install path as $PYTHON_INSTALL.

Fetching and building GNATcoll
''''''''''''''''''''''''''''''

Fetch GNATcoll sources:

    $ cd $WORKSPACE
    $ svn checkout svn+ssh://svn.eu.adacore.com/Dev/trunk/gps/gnatlib gnatcoll

Then execute the following commands:

    $ cd $WORKSPACE/gnatcoll
    $ ./configure --prefix=$PREFIX --disable-shared --disable-syslog \
        --with-python=$PYTHON_INSTALL --enable-projects --with-sqlite \
        --without-gmp
    $ make all install

Once GNATcoll has been built and installed, update the GPR_PROJECT_PATH:

    $ export GPR_PROJECT_PATH=$PREFIX/lib/gnat:$GPR_PROJECT_PATH

Building GNAThub
''''''''''''''''

Then execute the following command:

    $ cd $WORKSPACE/gnatdashboard/gnathub
    $ make
