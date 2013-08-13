===============================================================================
                Q u a l i m e t r i c s   D r i v e r 
===============================================================================

This directory contains Qualimetrics driver for GNAT tools suite.


It has 2 main parts:
~~~~~~~~~~~~~~~~~~~

    - The core -> Ada
    - Plugins  -> Python


It contains the following directories:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    - bin/       -->  Contains the main executable. 

    - obj/       -->  Object directory for the driver core.
 
    - share/     -->  Contains python part of Qualimetrics Driver, and the DB
                      schema description text file. 
                      N.B: This directory is used by the main executable, then
                           mandatory for Qualimetrics Driver installation.
 
    - src/       -->  Contains the source code of the driver core.

    - testsuite/ -->  Qualimetrics Driver test suite. Is not up to date !

