# GNAThub – GNATdashboard command line driver

GNAThub is a command-line tool that connects the dots between the GNAT Pro Tool
Suite and the GNATdashboard plugin for SonarQube. It collects and aggregates all
results from the various tools into a single place and stores those data into a
single SQLite file.

# Building GNAThub

## GNATcoll

GNAThub depends on the following GNATcoll modules:

  * Projects
  * SQLite
  * Python

Use the following switches to configure GNATcoll:

  $ ./configure --enable-projects --with-sqlite=embedded --with-python=<distrib>
  $ make
  $ make install

Depending on your needs, you might want to either explicitly disable all other
modules or enable only those you need at the `configure` step. Refer to GNATcoll
documentation for more information.

## Python

GNAThub embeds a Python distribution to execute its plugins. The distribution is
localized using the `python` interpreter available in the `PATH`, or provided
through the `PYTHON` variable to `make`.

GNAThub requires Python 2.7 (latest version recommended).

## GNAThub

Once GNATcoll has built and installed successfully, use GNAThub Makefile to
build the project:

  $ make

Use `BUILD_MODE` to build for `Debug` or `Production`:

  $ make BUILD_MODE=Debug

The default is `Production`.

Note that `gnatcoll_db2ada` tool is needed during the build to generate the
SQLite bindings.

## Installing GNAThub

Once build successfully, use the same Makefile to install GNAThub:

  $ make install

This will localize the Python distribution using the `PATH` and duplicate it to
`$(prefix)/share/gnathub/python`. Install your own distribution before install
to avoid this step.

If you wish to install in a different location than the default one, override
the `$(prefix)` variable:

  $ make prefix=/other/prefix install

The `Makefile` also support staged installs *via* the `DESTDIR` variable:

  $ make DESTDIR=/tmp/stage install
