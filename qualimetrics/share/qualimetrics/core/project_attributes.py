xml = """<?xml version="1.0"?>
    <Project_Support>

        <project_attribute
            name="Project_Name"
            label="Project name"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The name of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Plugin_Scheduling"
            label="Plugin Scheduling"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            list="true"
            description="Qualimetrics plugins execution order">
            <string />
        </project_attribute>

        <project_attribute
            name="Plugin_Off"
            label="Deactivated Plugin"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            list="true"
            description="Qualimetrics plugin to deactivate">
            <string />
        </project_attribute>

        <project_attribute
            name="Project_Version"
            label="Project Version"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The version of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Project_Key"
            label="Project Version"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="The key identifier of the project, as it
            will be reported to Sonar">
            <string />
        </project_attribute>

        <project_attribute
            name="Source_Encoding"
            label="Source Encoding"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            description="Encoding of the source files">
            <string />
        </project_attribute>

        <project_attribute
            name="Specific_Plugins"
            label="Project Specific Plugin"
            package="Qualimetrics"
            editor_page="Qualimetrics"
            editor_section="Naming"
            hide_in="wizard library_wizard"
            list="true"
            description="Path to project specific plugin for Qualimetrics">
            <string />
        </project_attribute>

</Project_Support>
"""

GPS.parse_xml(xml)

