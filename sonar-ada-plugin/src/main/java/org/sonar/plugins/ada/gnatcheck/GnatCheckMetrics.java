/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */

package org.sonar.plugins.ada.gnatcheck;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.utils.SonarException;

/**
 * Defines metrics specific to GNATcheck.
 */
public class GnatCheckMetrics implements Metrics{

    public static final GnatCheckMetrics INSTANCE = new GnatCheckMetrics();

    public static final String DOMAIN_CODING_STANDARD = "Coding standard";

    /**
     * Metrics to sum violations from last analysis
     */
    public static final Metric GNATCHECK_VIOLATIONS = new Metric.Builder("gnatcheck_violations", "GNATcheck violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric GNATCHECK_BLOCKER_VIOLATIONS = new Metric.Builder("gnatcheck_blocker_violations", "GNATcheck blocker violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck blocker violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric GNATCHECK_CRITICAL_VIOLATIONS = new Metric.Builder("gnatcheck_critical_violations", "GNATcheck critical violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck critical violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric GNATCHECK_MAJOR_VIOLATIONS = new Metric.Builder("gnatcheck_major_violations", "GNATcheck major violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck major violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric GNATCHECK_MINOR_VIOLATIONS = new Metric.Builder("gnatcheck_minor_violations", "GNATcheck minor violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck minor violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric GNATCHECK_INFO_VIOLATIONS = new Metric.Builder("gnatcheck_info_violations", "GNATcheck informational violations", Metric.ValueType.INT)
                        .setDescription("GNATcheck informational violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric GNATCHECK_WEIGHTED_VIOLATIONS = new Metric.Builder("gnatcheck_weighted_violations", "GNATcheck weighted violations", Metric.ValueType.INT)
                        .setHidden(true)
                        .setDescription("GNATcheck weighted violations")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create();

    public static final Metric GNATCHECK_VIOLATIONS_DENSITY = new Metric.Builder("gnatcheck_violations_density", "Coding standard compliance", Metric.ValueType.PERCENT)
                        .setDescription("Coding standard compliance")
                        .setDirection(Metric.DIRECTION_BETTER)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_CODING_STANDARD).create();

   private  static List<Metric> metrics = new ArrayList<Metric>();

   public List<Metric> getMetrics() {
    if (metrics.isEmpty()) {
      for (Field field : GnatCheckMetrics.class.getFields()) {
        if (Metric.class.isAssignableFrom(field.getType())) {
          try {
              metrics.add((Metric) field.get(null));
          } catch (IllegalAccessException e) {
            throw new SonarException("can not load metrics from " + GnatCheckMetrics.class.getSimpleName(), e);
          }
        }
      }
    }
    return metrics;
  }
}
