################################################################################
###                                    WHAT IS                               ###
################################################################################

Sonar Ada Plugin:

  Feeds Sonar with:
    - GNATcheck report information
    - GNATmetric report information


################################################################################
###                                     USAGE                                ###
################################################################################

How to install the plugin:

  - make sure you have a sonar installed on your computer
  - set the environment variable SONAR_HOME to the path to the Sonar home
  - run make 
  - restart Sonar


How to test the plugin:

  Example is provided in directory: qualimetrics/share/examples, for more 
  infromation, see the README in this directory.


How to run only the plugin test suite:

  As the plugin is buil with Maven, with the default behavior, test are run
  at the build. To by-pass the test suite execution you can run the following 
  command: 
            $ mvn clean install -Dmaven.test.skip=true
  Then to install the plugin, execute:
            $ cp target/sonar-ada-plugin-*.jar $SONAR_HOME/extensions/plugins/
  Otherwise, to only execute the test suite just run:
            $ make test

