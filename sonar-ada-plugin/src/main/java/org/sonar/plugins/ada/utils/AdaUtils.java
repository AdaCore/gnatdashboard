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
import org.sonar.plugins.ada.codepeer.CodepeerMetrics;

public final class AdaUtils {

    private AdaUtils() {
        // only static methods
    }
    /**
     * Default logger.
     */
    public static final Logger LOG = LoggerFactory.getLogger("AdaPlugin");

    public static Metric getMetricBySeverityAndCategory(String severity, String category) {
        for (Field field : CodepeerMetrics.class.getFields()) {
            if (Metric.class.isAssignableFrom(field.getType())) {
                try {
                    if (field.getName().contains(severity) && field.getName().contains(category)) {
                        return (Metric) field.get(null);
                    }
                } catch (IllegalAccessException e) {
                    throw new SonarException("While saving Codepeer metrics, cannot load metrics from " + CodepeerMetrics.class.getSimpleName(), e);
                }
            }
        }
        return null;
    }
}
