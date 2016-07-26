# Sonar Ada Plug-in rules definition generator

A SonarQube plug-in can define one or more rules, stored in a rules repository,
that can be later referenced and associated with issues.

For example, the sonar-ada-plugin can register, during its execution, an Issue
targetting a specific piece of code that violates a CodePeer given rule.

Those rules have to be defined by the plug-in and are registered during
SonarQube startup.

## Loading rules definition in SonarQube

In the context of the sonar-ada-plugin, rules definition are saved in XML files,
and loaded by the plug-in using SonarQube's `RulesDefinitionXmlLoader`.

These XML files are located in `<sonar-ada-plugin-root>/src/main/resources`:

  * codepeer.xml
  * gnatcheck.xml
  * gnatcoverage.xml

These files define new rules to register to SonarQube. They are not activated by
default. The sonar-ada-plugin provides a Quality Profile that lists rules to be
activated (in this case, all rules are activated by default). This Quality
Profile is also expressed in a XML file stored with the rules definitions:

  * default-profile.xml

## Generating XML assets

For maintenance purpose those XML files are, when possible, generated from the
tools documentation.

### codepeer.xml

CodePeer rules definition file is generated from the CodePeer documentation file
`messages_and_annotations.rst` using the `generate-codepeer-rules-definition`
script.

### gnatcheck.xml

For the moment the GNATcheck rules definition file is manually maintained
pending the transition of the documentation to the reST format.

### gnatcoverage.xml

GNATcoverage rules definition file is manually maintained given the little
number of rules it defines.

### default-profile.xml

The default Ada Quality Profile is generate from the above rules definition XML
files. All rules defined are activated by default.
