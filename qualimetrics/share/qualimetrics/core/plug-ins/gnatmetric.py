import GPS
import os
from xml.etree import ElementTree
from qmt_api.utils import OutputParser, create_parser, get_log_dir
from qmt_api.dao import save_resource_message, save_entity_message
from qmt_api.plugin import Tool

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


## GnatmetricOutputParser #####################################################
##
class GnatmetricOutputParser(OutputParser):
    """Define custom output parser"""
    def on_stdout(self,text):
        with open (Gnatmetric.get_log_file_path('gnatmetric'), 'w+a') as log:
            log.write(text)

## Gnatmetric ################################################################
##
class Gnatmetric(Tool):

    def __init__ (self):
        super(Gnatmetric, self).__init__('GNAT Metric')
        self.output_file_name='metrix.xml'

    def setup(self):
       GPS.parse_xml(xml_base)
       target = GPS.BuildTarget("GNAT Metrics for project and subprojects")
       target.execute()

    def parse_metrix_xml_file (self):
        object_dir = GPS.Project.root().object_dirs()[0]
        tree = ElementTree.parse(os.path.join(object_dir,
                                              self.output_file_name))
        for file_node in tree.findall('./file'):
            # Save file level metrics
            for metric in file_node.findall('./metric'):
                save_resource_message(self.name,
                                    file_node.attrib.get('name'),
                                    metric.attrib.get('name'),
                                    metric.text)
            # Save unit level metric
            for unit in file_node.findall('.//unit'):
                for metric in unit.findall('./metric'):
                    save_entity_message(self.name,
                                        file_node.attrib.get('name'),
                                        unit.attrib.get('line'),
                                        unit.attrib.get('name'),
                                        unit.attrib.get('col'),
                                        metric.attrib.get('name'),
                                        metric.text)

    def execute(self):
        self.parse_metrix_xml_file()



#output = GPS.get_build_output ("GNAT Metrics for project and subprojects", as_string=True)
#print (output)

