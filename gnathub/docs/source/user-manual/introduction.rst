.. include:: ../defines.hrst

Introduction
============

|GNATdashboard| is a tool for monitoring the software quality of Ada projects.

|GNATdashboard| fits in naturally a software development team’s workflow by using
projects files to configure, run, and analyse the ouput from GNAT Pro tools. The
driver program processes data such as compiler warning, CodePeer diagnostic
messages, style check violations, and coverage data, and makes it available for
reference and analysis through its local database.

The way |GNATdashboard| works is the following:

    * it runs tools coming from GNAT Pro tool suite (such as |GNATcheck|,
      |GNATmetric|, |GNATcoverage|, |CodePeer|, *etc.*)

    * it processes the information coming from tools and collects it
      in a normalized database

    * it can read data from the normalized database and feed it to
      high-level platforms such as |SonarQube| and |Squoring|.

|GNATdashboard| is available on the following platforms:

    * Windows (32-bits)
    * Linux (32-bits and 64-bits)
    * OS X 10.9 (64-bits)

Prerequisites
-------------

In order to use |GNATdashboard| you need:

    * |GNAT| tools you intend to run on your project (|GNATmetric|, |GNATcheck|,
      ...)

    * a |GNAThub| installation


Release
-------

The current version is |release|.


Installation
------------

|GNAThub| can be downloaded off |GNATtracker| as part of the |GNATdashboard|
package. Retrieve this package using your |GNATtracker| account.

On Windows, run the graphical installer. On other platforms, un-zip the
downloaded archive and install it on your system (usually at some place such as
``/usr/local/gnatpro``).


Setting your environment
------------------------

In order to use |GNAThub| you need to have all the tools you are planning to use
on your project in your ``$PATH`` (some tools come with the |GNATpro|
distribution, others are available as separate packages).
