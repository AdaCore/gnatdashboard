/*
 * Sonar Requirements Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.requirements;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.MeanAggregationFormula;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.utils.SonarException;

/**
 * Define specific metrics for project requirements.
 */
public class RequirementsMetrics implements Metrics {

    // Defines domain for requirements
    public static final String DOMAIN_REQUIREMENTS = "Requirements";

    public static final Metric EXP_REQS = new Metric.Builder("exp_reqs", "Expected requirements", Metric.ValueType.INT)
            .setDescription("The number of requirements a project manager expects his team to develop."
            + "This number may be calculated based on some specific input (number of subprograms,"
            + "number of Simulink blocks, ...) or estimated.")
            .setDirection(Metric.DIRECTION_NONE)
            .setQualitative(Boolean.FALSE)
            .setDomain(DOMAIN_REQUIREMENTS).setFormula(new SumChildValuesFormula(true))
            .setFormula(new SumChildValuesFormula(false))
            .create();

    public static final Metric DEV_REQS = new Metric.Builder("dev_reqs", "Developped requirements (%)", Metric.ValueType.PERCENT)
            .setDescription("The percentage of requirements already developed")
            .setDirection(Metric.DIRECTION_BETTER)
            .setQualitative(Boolean.FALSE)
            .setDomain(DOMAIN_REQUIREMENTS)
            .setFormula(new MeanAggregationFormula())
            .create();

    public static final Metric UNTRACEABLE_SUBPROGRAMS = new Metric.Builder("untraceable_subprograms", "Untraceable subprograms (%)", Metric.ValueType.PERCENT)
            .setDescription("The percentage of subprograms that are not traceable to a requirement")
            .setDirection(Metric.DIRECTION_WORST)
            .setQualitative(Boolean.FALSE)
            .setDomain(DOMAIN_REQUIREMENTS)
            .setFormula(new MeanAggregationFormula())
            .create();

    private static List<Metric> metrics = new ArrayList<Metric>();

    /**
     * Defined in the Metrics interface and is used by Sonar to retrieve the
     * list of new metrics.
     */
    @Override
    public List<Metric> getMetrics() {
        if (metrics.isEmpty()) {
            for (Field field : RequirementsMetrics.class.getFields()) {
                if (Metric.class.isAssignableFrom(field.getType())) {
                    try {
                        metrics.add((Metric) field.get(null));
                    } catch (IllegalAccessException e) {
                        throw new SonarException("can not load metrics from " +
                                RequirementsMetrics.class.getSimpleName(), e);
                    }
                }
            }
        }
        return metrics;
    }

}
