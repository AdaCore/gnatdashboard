/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;

/**
 * Defines metrics specific to GnatMetric that cannot be
 * bind to existent Sonar's metric.
 */
public final class GnatMetrics implements Metrics {

    public final static GnatMetrics INSTANCE = new GnatMetrics();
    public static final Metric EOL_COMMENT = new Metric.Builder("eol_comments", "End of line comments", Metric.ValueType.INT)
            .setDescription("Identified all end of line comments")
            .setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_GENERAL)
            .create()
            .setFormula(new SumChildValuesFormula(false));

    public static final Metric BLANK_LINE = new Metric.Builder("blank_lines", "Blank lines", Metric.ValueType.INT)
            .setDescription("Identified blank lines")
            .setDirection(Metric.DIRECTION_WORST)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_GENERAL)
            .create().
            setFormula(new SumChildValuesFormula(false));

    public static List<Metric> metrics = new ArrayList<Metric>();

    /**
     * Is defined in the Metrics interface and is used by
     * Sonar to retrieve the list of new metrics.
     */
    public List<Metric> getMetrics() {
        if (metrics.isEmpty()) {
            metrics.add(EOL_COMMENT);
            metrics.add(BLANK_LINE);
        }
        return metrics;
    }
}
