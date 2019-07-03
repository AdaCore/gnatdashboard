.. include:: ../defines.hrst

Introduction
============

|GNATdashboard| is a product for monitoring the software quality of Ada
projects.

|GNATdashboard| fits naturally into a software development team's workflow
by leveraging on project files to configure, run, and analyze the output from
|GNAT| tools. Its driver program processes data such as compiler warnings,
|CodePeer| diagnostic messages, style check violations, and coverage data,
and makes it available for reference and analysis through its local
database.

|GNATdashboard| works as follows:

* it runs tools from GNAT Pro tool suite such as |GNATcheck|, |GNATmetric|,
  |GNATcoverage| and additional tools like |CodePeer|

* it processes the output from those tools and collects it in a database

* it extracts data from the database and feeds it to high-level platforms
  such as |SonarQube| and |Squoring|

|GNATdashboard| is available on the following platforms:

* Windows (32-bits and 64-bits)
* Linux (32-bits and 64-bits)
* OS X 10.10 (64-bits)

.. _ug-intro-prerequisites:

Prerequisites
-------------

To use |GNATdashboard| you must have both the |GNAT| tools you intend to
run on your project (such as |GNATmetric| and |GNATcheck|) and |GNAThub|
installed.

In order to visualize results in the |SonarQube| platform, the corresponding
plug-in must be deployed.

|SonarAdaPlugin| is supported for versions of |SonarQube| and |SonarScanner|
supported by |SonarSource|:

  * for |SonarQube| |SonarQubeLTSVersion| LTS (Long Term Support)
  * |SonarScanner| 3.0.3

Upgrading from GNATdashboard 1.0
--------------------------------

Various breaking changes to the |SonarAdaPlugin| were mandatory to adapt to the
new |SonarQube| plug-in API and support the latest versions of |SonarQube| (from
LTS to stable). Is this regard, the |SonarAdaPlugin| is no longer compatible nor
supported on versions of |SonarQube| prior to |SonarQubeLTSVersion|.

Note that |GNATdashboard| 1.1.x is required for use with the latest version of
the |SonarAdaPlugin|.

Release
-------

The current version is |release|.

Installation
------------

Download the |GNATdashboard| package using your |GNATtracker| account.

On Windows, run the graphical installer. On other platforms, un-zip the
downloaded archive and install it on your system (usually at some location
such as :file:`/usr/local/gnatpro`) under a new folder. In order to be able
to use it, you should add to your :envvar:`$PATH` the :file:`/bin/` folder
of your |GNATdashboard| installation.

The |SonarQube| plug-in is located in the directory
:file:`<install_prefix>/share/sonar`. This needs to be installed manually.
In order to do that, you should

  * stop the |SonarQube| server
  * copy the Sonar Ada plugin from |GNATdashboard| installation repository
    subfolder :file:`/share/sonar/extensions/plugins/` into the |SonarQube|
    installation repository under :file:`/extensions/plugins/`
  * restart the |SonarQube| server.

.. note::
   It is very important that only one |Sonar Ada plugin| be present in this
   repository and it needs to match the |GNATdashboard| version that you just
   installed. Any older version of this plugin must be removed before restarting
   |SonarQube| server.

Setting your environment
------------------------

To use |GNAThub| you must have all the tools you are planning to use on
your project in your :envvar:`$PATH` (some tools come with the |GNAT|
distribution, while others are available as separate packages).
