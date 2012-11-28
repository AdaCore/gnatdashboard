/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.utils;

import java.lang.reflect.Field;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.measures.Metric;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.codepeer.CodePeerMetrics;

public final class AdaUtils {

    private AdaUtils() {
        // only static methods
    }
    /**
     * Default logger.
     */
    public static final Logger LOG = LoggerFactory.getLogger("AdaPlugin");

     /**
      * Retrieves Metric for violations for a given repository, category and severity.
      * Only category can be null.
      *
      * @param repositoryKey
      * @param severity
      * @param category
      * @return Metric
      */
     public static Metric getMetricByRuleRepoSeverityAndCategory(String ruleRepoKey, String className, String severity, String category) {
        try {
            Class plugin = Class.forName("org.sonar.plugins.ada." + ruleRepoKey + "." + className);
            for (Field field : plugin.getFields()) {
                if (Metric.class.isAssignableFrom(field.getType())) {
                    try {
                        if ((category == null && field.getName().contains(severity)) ||
                                (category != null &&field.getName().contains(severity) &&
                                 field.getName().contains(category))) {
                            return (Metric) field.get(null);
                        }
                    } catch (IllegalAccessException e) {
                        throw new SonarException("While saving Codepeer metrics, cannot load metrics from " + CodePeerMetrics.class.getSimpleName(), e);
                    }
                }
            }
            return null;
        } catch (ClassNotFoundException ex) {
            throw new SonarException("Plugin metric class not found with rule repository key and class name: " + ruleRepoKey + ", " +className, ex);
        }
    }
}
