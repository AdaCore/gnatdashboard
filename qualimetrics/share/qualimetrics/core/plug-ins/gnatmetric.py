import GPS
import os
from xml.etree import ElementTree
from qualimetrics_api import OutputParser, create_parser, save_resource_message, save_entity_message

OUTPUT_FILE_NAME='metrix.xml'
LOG_FILE_NAME='gnatmetric.log'

# imitate tool_output module:
class tool_output ():
    @staticmethod
    def create_parser (name, child=None):
        return create_parser (name, child)

# Define custom output parser:
class GNATMetricParser(OutputParser):
    def on_stdout(self,text):
        object_dir = GPS.Project.root().object_dirs()[0]
        with open (os.path.join(object_dir, LOG_FILE_NAME), 'w+a') as log:
            log.write(text)

# Parse metrix.xml file
def parse_metrix_xml_file ():
    object_dir = GPS.Project.root().object_dirs()[0]
    tree = ElementTree.parse(os.path.join(object_dir, OUTPUT_FILE_NAME))
    for file_node in tree.findall('./file'):
        # Save file level metrics
        for metric in file_node.findall('./metric'):
            save_resource_message(file_node.attrib.get('name'),
                                metric.attrib.get('name'),
                                metric.text)
        # Save unit level metric
        for unit in file_node.findall('.//unit'):
            for metric in unit.findall('./metric'):
                save_entity_message(file_node.attrib.get('name'),
                                    unit.attrib.get('line'),
                                    unit.attrib.get('name'),
                                    unit.attrib.get('col'),
                                    metric.attrib.get('name'),
                                    metric.text)

# Initialize the targets
xml_base = """<?xml version="1.0"?>
<GPS>
 <builder-mode name="default">
  <description>Build with default switches defined in the project</description>
 </builder-mode>

<target-model name="gnatmetrics" category="">
   <description>Generic launch of gnat metrics</description>
   <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
   </command-line>
</target-model>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>%F</arg>
    </command-line>
</target>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for project">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
    </command-line>
</target>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for project and subprojects">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-ox</arg>
      <arg>%O/metrix.xml</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>-U</arg>
    </command-line>
   <output-parsers>gnatmetricparser output_collector</output-parsers>
</target>

</GPS>
"""

GPS.parse_xml(xml_base)
target = GPS.BuildTarget("GNAT Metrics for project and subprojects")
target.execute()
#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
#print (output)
parse_metrix_xml_file()

