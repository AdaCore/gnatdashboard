.. include:: ../defines.hrst

Introduction
============

|Product| fits in naturally with a software development team’s workflow by using
projects files to configure, run, and analyse the ouput from AdaCore tools. The
driver program processes data such as compiler warning, CodePeer diagnostic
messages, style check violations, and coverage data, and makes it available for
reference and analysis through its local database.

The |Product| project has two main purposes:

    * Retrieve output from GNAT Pro tool suite (such as |GNATcheck|,
      |GNATmetric|, |GNATcoverage|, |CodePeer|, *etc.*) so as to produce a
      common output;
    * Process this information and integrate it to higher-level platforms
      (|SonarQube|, |Squoring|, ...).

|Product| is available on the following platforms:

    * Windows (x86 and x86_64)
    * Linux (x86 and x86_64)
    * OS X 10.9 (x86_64)

Prerequisites
-------------

In order to use |Product| you need:

    * |GNAT| tools you intend to run on your project (|GNATmetric|, |GNATcheck|,
      ...)
    * a |Driver| installation


Release
-------

The current beta version is |release|.


Installation
------------

|Driver| can be downloaded off |GNATtracker| as part of the |Product| package.
Retrieve this package using your |GNATtracker| account.

On Windows, run the graphical installer. On other platforms, un-zip the
downloaded archive and install it on your system (usually at some place such as
``/usr/local/gnatpro``).


Setting your environment
------------------------

In order to use |Driver| you need to have all the tools you are planning to use
on your project in your ``$PATH`` (some tools come with the |GNATpro|
distribution, others are available as separate packages).
