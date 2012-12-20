/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.Extension;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.plugins.ada.codepeer.AdaCodePeerRuleRepository;
import org.sonar.plugins.ada.codepeer.AdaCodePeerSensor;
import org.sonar.plugins.ada.codepeer.CodePeerMetrics;
import org.sonar.plugins.ada.codepeer.CodePeerViolationsDecorator;
import org.sonar.plugins.ada.codepeer.gcov.GCovSensor;
import org.sonar.plugins.ada.gnatcheck.AdaGnatCheckRuleRepository;
import org.sonar.plugins.ada.gnatcheck.AdaGnatCheckSensor;
import org.sonar.plugins.ada.gnatcheck.GnatCheckMetrics;
import org.sonar.plugins.ada.gnatcheck.GnatCheckViolationsDecorator;
import org.sonar.plugins.ada.gnatcheck.GnatCheckWeightedViolationsDecorator;
import org.sonar.plugins.ada.gnatcheck.GnatCheckViolationsDensityDecorator;
import org.sonar.plugins.ada.gnatmetric.AdaGnatMetricSensor;
import org.sonar.plugins.ada.gnatmetric.GnatMetrics;
import org.sonar.plugins.ada.ui.gwt.GnatMetricViewerDefinition;
import org.sonar.plugins.ada.ui.rubyWidget.CodePeerViolationsRubyWidget;
import org.sonar.plugins.ada.ui.rubyWidget.GnatCheckViolationsRubyWidget;

@Properties({
    @Property(key = AdaSourceImporter.PROJECT_TREE_FILE_PATH_KEY,
        name = "Project tree file",
        description = "Path to file which contains project tree in JSON format. Relative to the project root",
        global = false,
        project = true),
    @Property(key = AdaGnatCheckSensor.REPORT_PATH_KEY,
        name = "Path to GNATcheck report",
        description = "Relative to the project root",
        global = false,
        project = true),
    @Property(key = AdaGnatMetricSensor.REPORT_PATH_KEY,
        name = "Path to GNATmetric report",
        description = "Relative to the project root",
        global = false,
        project = true),
    @Property(key = AdaCodePeerSensor.REPORT_PATH_KEY,
        name = "Path to Codepeer report",
        description = "Relative to the project root",
        global = false,
        project = true),
    @Property(key = GCovSensor.REPORT_PATH_KEY,
        name = "Path to gcov report",
        description = "Relative to the project root",
        global = false,
        project = true)})
/**
 * Implements Ada Plugin for Sonar
 */
public final class AdaPlugin extends SonarPlugin {

    /**
     * Returns Sonar Extensions used into the plugin
     *
     * @return the classes to use into the plugin
     */
    public List getExtensions() {
        List<Class<? extends Extension>> l = new ArrayList<Class<? extends Extension>>();
        l.add(Ada.class);
        l.add(AdaSourceImporter.class);
        l.add(AdaDefaultProfile.class);
        l.add(AdaColorizer.class);

        //CodeePeer
        l.add(AdaCodePeerSensor.class);
        l.add(AdaCodePeerRuleRepository.class);
        l.add(CodePeerMetrics.class);
        l.add(CodePeerViolationsRubyWidget.class);
        l.add(CodePeerViolationsDecorator.class);

        //GNATcheck
        l.add(AdaGnatCheckRuleRepository.class);
        l.add(AdaGnatCheckSensor.class);
        l.add(GnatCheckMetrics.class);
        l.add(GnatCheckViolationsRubyWidget.class);
        l.add(GnatCheckViolationsDecorator.class);
        l.add(GnatCheckViolationsDensityDecorator.class);
        l.add(GnatCheckWeightedViolationsDecorator.class);

        //GNATmetric
        l.add(AdaGnatMetricSensor.class);
        l.add(GnatMetricViewerDefinition.class);
        l.add(GnatMetrics.class);

        //GCov
        l.add(GCovSensor.class);

        return l;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
