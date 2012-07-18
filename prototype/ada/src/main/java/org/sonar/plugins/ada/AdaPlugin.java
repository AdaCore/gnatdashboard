/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.Extension;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.plugins.ada.gnatcheck.AdaGnatCheckRuleRepository;
import org.sonar.plugins.ada.gnatcheck.AdaGnatCheckSensor;

@Properties({
    @Property(key = "sonar.ada.gnatmetrics.reportPath",
        name = "Path to gnatmetrics report",
        description = "Relative to the project root",
        global = false,
        project = true),
    @Property(key = AdaGnatCheckSensor.REPORT_PATH_KEY,
        name = "Path to gnatcheck report",
        description = "Relative to the project root",
        global = false,
        project = true),
    @Property(
      key = AdaPlugin.FILE_SUFFIXES_KEY,
      defaultValue = "adb,ads",
      name = "File suffixes",
      description = "Comma-separated list of suffixes for files to analyze. Leave empty to use the default.",
      global = true,
      project = true),})
public final class AdaPlugin extends SonarPlugin {

    static final String FILE_SUFFIXES_KEY = "sonar.ada.suffixes";

    public List getExtensions() {
        List<Class<? extends Extension>> l = new ArrayList<Class<? extends Extension>>();
        l.add(Ada.class);
        l.add(AdaSourceImporter.class);
        l.add(AdaCpdMapping.class);
        l.add(AdaGnatCheckRuleRepository.class);
        l.add(AdaGnatCheckSensor.class);
        l.add(AdaDefaultProfile.class);
        return l;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
