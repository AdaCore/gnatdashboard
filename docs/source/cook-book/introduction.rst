.. include:: ../defines.hrst

Getting Started with |GNATdashboard|
====================================

Prerequisites
-------------

  * a |SonarRunner| installation configured for the targeted |SonarQube|
    instance, and in your :envvar:`$PATH` (see :ref:`ug-intro-prerequisites`
    for more information on supported versions of |SonarQube| and
    |SonarRunner|).

  * a |GNATPro| installation (the most recent installed on the system), and in
    your :envvar:`$PATH`

  * *(optional, and recommended)* a |CodePeer| installation, in your
    :envvar:`$PATH`

|GNATdashboard| setup
---------------------

  * *(on Windows)* run the installer, and place :file:`<install_prefix>\bin` in
    your :envvar:`$PATH`

  * *(on \*NIX)* extract the installation directory from the archive, and place
    :file:`<install_prefix>/bin` in your :envvar:`$PATH`

  * *(on all platforms)* copy
    :file:`<install_prefix>/share/sonar/extensions/plugins/*.jar` into the
    equivalent place in your |SonarQube| installation, and restart |SonarQube|

The 5-lines manual to |GNATdashboard|
-------------------------------------

|GNATdashboard| contains a driver program, |GNAThub|, which:

  * executes all |GNAT| tools and stores the results in a database

  * creates a configuration file for your project ready to use by
    :program:`sonar-runner` (:file:`sonar-project.properties`)

  * launches the :program:`sonar-runner`


The |SonarRunner| is reading the results from the database created by the
driver |GNAThub|.

For more information, the full manual is available at:

  * :file:`<install_prefix>/share/doc/gnatdashboard/html`

  * :file:`<install_prefix>/share/doc/gnatdashboard/pdf`

And online at https://docs.adacore.com/gnatdashboard-docs/.

Trying with an example
----------------------

A complete example is provided at:

  * :file:`<install_prefix>/share/example/gnatdashboard/sdc`

This contains an Ada project and a :file:`Makefile` which builds the project,
computes coverage information using |Gcov|, then launches the |GNAThub| driver.

The :file:`Makefile` also launches a Python script which does a simple textual
dump of the contents of the database - this is to demonstrate the scripting
capabilities of the |GNATdashboard| driver.
