.. include:: ../defines.hrst

Introduction
============

|GNATdashboard| is a tool that monitors the software quality of Ada projects.

|GNATdashboard| fits naturally into a software development team's workflow
by using projects files to configure, run, and analyze the ouput from
|GNAT| tools. Its driver program processes data such as compiler warnings,
|CodePeer| diagnostic messages, style check violations, and coverage data,
and makes it available for reference and analysis through its local
database.

|GNATdashboard| works as follows:

* it runs tools from GNAT Pro tool suite such as |GNATcheck|, |GNATmetric|,
  |GNATcoverage|, and |CodePeer|

* it processes the output from those tools and collects it in a database

* it extracts data from the database and feeds it to high-level platforms
  such as |SonarQube| and |Squoring|

|GNATdashboard| is available on the following platforms:

* Windows (32-bits)
* Linux (32-bits and 64-bits)
* OS X 10.9 (64-bits)

Prerequisites
-------------

To use |GNATdashboard| you must have both the |GNAT| tools you intend to
run on your project (such as |GNATmetric| and |GNATcheck|) and |GNAThub|
installed.

Release
-------

The current version is |release|.


Installation
------------

Download |GNAThub| from |GNATtracker| as part of the |GNATdashboard|
package using your |GNATtracker| account.

On Windows, run the graphical installer. On other platforms, un-zip the
downloaded archive and install it on your system (usually at some location
such as :file:`/usr/local/gnatpro`).


Setting your environment
------------------------

To use |GNAThub| you must have all the tools you are planning to use on
your project in your :envvar:`$PATH` (some tools come with the |GNAT|
distribution, while others are available as separate packages).
