.. include:: ../defines.hrst

Frequently Asked Questions
==========================

SonarQube doesn't report CodePeer/GNATcheck rules violations
------------------------------------------------------------

If SonarQube does not display issues reported by CodePeer and/or GNATcheck
despite of GNAThub running the tools and collecting the results, make sure all
required rules are activated in the `Quality Profile <http://docs.sonarqube.org/display/SONAR/Quality+Profiles>`_
you are using for Ada sources. 

The SonarQube Ada Plugin that comes with GNATdashboard provides its own quality
profile called "GNATdashboard Way". This profile is now the default for Ada
projects and supports the latest additions to |CodePeer| and |GNATcheck|.
