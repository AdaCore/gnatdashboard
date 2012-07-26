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
    @Property(key = AdaSourceImporter.PROJECT_TREE_FILE_PATH_KEY,
    name = "Project tree file",
    description = "Path to file which contains project tree in JSON format. Relative to the project root",
    global = false,
    project = true),
    @Property(key = AdaGnatCheckSensor.REPORT_PATH_KEY,
    name = "Path to gnatcheck report",
    description = "Relative to the project root",
    global = false,
    project = true)})
public final class AdaPlugin extends SonarPlugin {

    public List getExtensions() {
        List<Class<? extends Extension>> l = new ArrayList<Class<? extends Extension>>();
        l.add(Ada.class);
        l.add(AdaSourceImporter.class);
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
