/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.MeanAggregationFormula;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.utils.MaxAggregationFormula;

/**
 * Defines metrics specific to GnatMetric that cannot be bind to existent
 * Sonar's metric.
 */
public final class GnatMetrics implements Metrics {

    public enum Metrics {

    // Documentation
    COMMENT_LINES(CoreMetrics.COMMENT_LINES),
    COMMENT_PERCENTAGE(CoreMetrics.COMMENT_LINES_DENSITY),
    BLANK_LINES(GnatMetrics.BLANK_LINE),
    EOL_COMMENTS(GnatMetrics.EOL_COMMENT),
    // Size
    ALL_LINES(CoreMetrics.LINES),
    CODE_LINES(CoreMetrics.NCLOC),
    LSLOC(GnatMetrics.LSLOC),
    //Complexity
    CYCLOMATIC_COMPLEXITY(CoreMetrics.COMPLEXITY),
    STATEMENT_COMPLEXITY(GnatMetrics.STATEMENT_COMPLEXITY),
    EXPRESSION_COMPLEXITY(GnatMetrics.EXPRESSION_COMPLEXITY),
    ESSENTIAL_COMPLEXITY(GnatMetrics.ESSENTIAL_COMPLEXITY),
    MAX_LOOP_NESTING(GnatMetrics.MAX_LOOP_NESTING);

    private final Metric sonarMetric;

    private Metrics(Metric sonarMetric) {
        this.sonarMetric = sonarMetric;
    }

    public Metric getMetric() {
        return this.sonarMetric;
    }

    public String key() {
        return this.name().toLowerCase();
    }
}
    public static final GnatMetrics INSTANCE = new GnatMetrics();

    //----------------
    //     SIZE
    //----------------
    public static final Metric LSLOC = new Metric.Builder("lsoc", "Logical source line of code", Metric.ValueType.INT)
            .setDescription("Logical source line of code").setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));
    public static final Metric BLANK_LINE = new Metric.Builder("blank_lines", "Blank lines", Metric.ValueType.INT)
            .setDescription("Identified blank lines").setDirection(Metric.DIRECTION_WORST)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));
    //----------------
    //  DOCUMENTATION
    //----------------
    public static final Metric EOL_COMMENT = new Metric.Builder("eol_comments", "End of line comments", Metric.ValueType.INT)
            .setDescription("Identified all end of line comments")
            .setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_DOCUMENTATION).create().setFormula(new SumChildValuesFormula(false));
    //----------------
    //   COMPLEXITY
    //----------------
    public static final Metric STATEMENT_COMPLEXITY = new Metric.Builder("statement_complexity", "Statement complexity", Metric.ValueType.FLOAT)
            .setDescription("Average of all statements complexity in the file").setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create()
            .setFormula(new MeanAggregationFormula(true));
    public static final Metric EXPRESSION_COMPLEXITY = new Metric.Builder("expression_complexity", "Expression complexity", Metric.ValueType.FLOAT)
            .setDescription("Expression complexity").setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create()
            .setFormula(new MeanAggregationFormula(true));
    public static final Metric ESSENTIAL_COMPLEXITY = new Metric.Builder("essential_complexity", "Essential complexity", Metric.ValueType.FLOAT)
            .setDescription("Essential complexity").setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create()
            .setFormula(new MeanAggregationFormula(true));
    public static final Metric MAX_LOOP_NESTING = new Metric.Builder("max_loop_nesting", "Maximum loop nesting", Metric.ValueType.INT)
            .setDescription("Maximum loop nesting").setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create()
            .setFormula(new MaxAggregationFormula());
    private static List<Metric> metrics = new ArrayList<Metric>();

    /**
     * Is defined in the Metrics interface and is used by Sonar to retrieve the
     * list of new metrics.
     */
    public List<Metric> getMetrics() {
        if (metrics.isEmpty()) {
            for (Field field : GnatMetrics.class.getFields()) {
                if (Metric.class.isAssignableFrom(field.getType())) {
                    try {
                        metrics.add((Metric) field.get(null));
                    } catch (IllegalAccessException e) {
                        throw new SonarException("can not load metrics from " + GnatMetrics.class.getSimpleName(), e);
                    }
                }
            }
        }
        return metrics;
    }
}
