/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.Extension;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.codepeer.AdaCodePeerRuleRepository;
import org.sonar.plugins.ada.codepeer.CodePeerMetrics;
import org.sonar.plugins.ada.codepeer.CodePeerViolationsDecorator;
import org.sonar.plugins.ada.gcov.GcovSensor;
import org.sonar.plugins.ada.gnatcheck.AdaGnatCheckRuleRepository;
import org.sonar.plugins.ada.gnatcheck.GnatCheckMetrics;
import org.sonar.plugins.ada.gnatcheck.GnatCheckViolationsDecorator;
import org.sonar.plugins.ada.gnatcheck.GnatCheckViolationsDensityDecorator;
import org.sonar.plugins.ada.gnatcheck.GnatCheckWeightedViolationsDecorator;
import org.sonar.plugins.ada.gnatmetric.AdaGnatMetricSensor;
import org.sonar.plugins.ada.gnatmetric.GnatMetrics;
import org.sonar.plugins.ada.persistence.JDBCUtils;
import org.sonar.plugins.ada.ui.gwt.GnatMetricViewerDefinition;
import org.sonar.plugins.ada.ui.rubyWidget.CodePeerViolationsRubyWidget;
import org.sonar.plugins.ada.ui.rubyWidget.GnatCheckViolationsRubyWidget;

@Properties({
    @Property(key = JDBCUtils.QMT_DB_PATH,
    name = "Path to qualimetrics database",
    description = "Qualimetrics DB relative path to the project root",
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
    @Override
    public List getExtensions() {
        List<Class<? extends Extension>> l = new ArrayList<Class<? extends Extension>>();

        l.add(Ada.class);
        l.add(AdaSourceImporter.class);
        l.add(AdaDefaultProfile.class);
        l.add(AdaColorizer.class);

        // Issues
        l.add(GNATIssuesSensor.class);

        // CodeePeer
        l.add(AdaCodePeerRuleRepository.class);
        l.add(CodePeerMetrics.class);
        l.add(CodePeerViolationsRubyWidget.class);
        l.add(CodePeerViolationsDecorator.class);

        // GNATcheck
        l.add(AdaGnatCheckRuleRepository.class);
        l.add(GnatCheckMetrics.class);
        l.add(GnatCheckViolationsRubyWidget.class);
        l.add(GnatCheckViolationsDecorator.class);
        l.add(GnatCheckViolationsDensityDecorator.class);
        l.add(GnatCheckWeightedViolationsDecorator.class);

        // GNATmetric
        l.add(AdaGnatMetricSensor.class);
        l.add(GnatMetricViewerDefinition.class);
        l.add(GnatMetrics.class);

        // Gcov
        l.add(GcovSensor.class);

        return l;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
