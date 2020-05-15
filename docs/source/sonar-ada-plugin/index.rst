.. include:: ../defines.hrst

Sonar Ada Plugin
================

This SonarQube plugin loads the analysis of Ada projects by GNATdashboard.

ChangeLog 21.x
--------------

The Sonar Ada Plugin shipped with GNATdashboard now supports |SonarQube|
|SonarQubeLTSVersion| (LTS).

Because of that, it also drops support for old versions of |SonarQube| (*i.e.*
all versions *pre* |SonarQubeLTSVersion|).

Follow `SonarSource's procedure <http://docs.sonarqube.org/display/SONAR/Upgrading>`_
to update |SonarQube| to a supported version.

SonarQube Scanner
'''''''''''''''''

|SonarSource| replaced |SonarRunner| by the new
`SonarSource Scanner <http://docs.sonarqube.org/display/SCAN/Analyzing+with+SonarQube+Scanner>`_
to analyse a project.

GNATdashboard dropped support for |SonarRunner|, and is now compatible with
|SonarScanner| version 4.0, recommended as the default launcher for |SonarQube|
|SonarQubeLTSVersion| (LTS).

Install |SonarScanner| make it available in your :envvar:`PATH` prior to
executing |GNAThub| on your projects.

AdaCore tools support
'''''''''''''''''''''

|CodePeer|, |GNATcheck| and |GNATmetric| now better integrate with |SonarQube|:

* a simplified `Quality Profile <http://docs.sonarqube.org/display/SONAR/Quality+Profiles>`_
  called "GNATdashboard Way" is now the default for Ada projects and supports
  the latest additions to |CodePeer|;
* |CodePeer| race condition messages are marked as BUG during the analysis while
  other messages remain tagged as CODE SMELL to match |SonarQube| jargon;
* suppressed messages are now filtered out of the analysis so that |SonarQube|
  can mark them as "Resolved";
* |GNATmetric| metrics are mapped to |SonarQube| metrics when they share the
  same definition, and custom metrics are better defined to allow |SonarQube| to
  qualify the trend of a project ("is the quality improving or not?").
