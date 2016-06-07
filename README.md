# GNATdashboard

GNATdashboard contains a driver program, GNAThub, which:

* executes all GNAT tools and stores the results in a database
* creates a configuration file for your project ready to use by sonar-runner
  (`sonar-project.properties`)
* launches the sonar-runner

The SonarQube Runner is reading the results from the database created by the
driver GNAThub.

For more information, the full manual is available at:

* `<install_prefix>/share/doc/gnatdashboard/html`
* `<install_prefix>/share/doc/gnatdashboard/pdf`

See also the [online docs](https://docs.adacore.com/gnatdashboard-docs/).
