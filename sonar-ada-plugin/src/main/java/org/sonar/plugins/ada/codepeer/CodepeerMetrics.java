/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */

package org.sonar.plugins.ada.codepeer;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.utils.SonarException;

/**
 * Defines metrics specific to Codepeer.
 */
public class CodepeerMetrics implements Metrics{

    //Instance used by the plugin
    public static final CodepeerMetrics INSTANCE = new CodepeerMetrics();

    public static final String DOMAIN_STATIC_ANALYSIS = "Static analysis";

    /**
     * Metrics to sum violations from last analysis
     */


    public static final Metric CODEPEER_VIOLATIONS = new Metric.Builder("codepeer_violations", "Codepeer messages", Metric.ValueType.INT)
                        .setDescription("Codepeer messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_INFORMATIONAL_INFO_VIOLATIONS = new Metric.Builder("codepeer_informational_info_violations", "Codepeer informational messages", Metric.ValueType.INT)
                        .setDescription("Codepeer informational messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_CHECK_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_check_critical_violations", "Codepeer check high messages", Metric.ValueType.INT)
                        .setDescription("Codepeer check high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_CHECK_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_check_major_violations", "Codepeer check medium messages", Metric.ValueType.INT)
                        .setDescription("Codepeer check medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_CHECK_MINOR_VIOLATIONS = new Metric.Builder("codepeer_check_minor_violations", "Codepeer check low messages", Metric.ValueType.INT)
                        .setDescription("Codepeer check low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_warning_critical_violations", "Codepeer warning high messages", Metric.ValueType.INT)
                        .setDescription("Codepeer warning high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_warning_major_violations", "Codepeer warning medium messages", Metric.ValueType.INT)
                        .setDescription("Codepeer warning medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_MINOR_VIOLATIONS = new Metric.Builder("codepeer_warning_minor_violations", "Codepeer warning low messages", Metric.ValueType.INT)
                        .setDescription("Codepeer warning low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_race_condition_critical_violations", "Codepeer race condition high messages", Metric.ValueType.INT)
                        .setDescription("Codepeer race condition high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_race_condition_major_violations", "Codepeer race condition medium messages", Metric.ValueType.INT)
                        .setDescription("Codepeer race condition medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_MINOR_VIOLATIONS = new Metric.Builder("codepeer_race_condition_minor_violations", "Codepeer race condition low messages", Metric.ValueType.INT)
                        .setDescription("Codepeer race condition low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    /**
     * Delta of violations with previous analysis
     *  Commented for now, because may be not useful
     */
//    public static final Metric NEW_CODEPEER_VIOLATIONS = new Metric.Builder("new_codepeer_critical_violations", "New Codepeer critical violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer critical violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_CRITCAL_VIOLATIONS = new Metric.Builder("new_codepeer_critical_violations", "New Codepeer critical violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer critical violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_MAJOR_VIOLATIONS = new Metric.Builder("new_codepeer_major_violations", "New Codepeer major violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer major violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_MINOR_VIOLATIONS = new Metric.Builder("new_codepeer_low_violations", "New Codepeer minor violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer minor violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_INFO_VIOLATIONS = new Metric.Builder("new_codepeer_info_violations", "New Codepeer informational violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer informational violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_CHECK_VIOLATIONS = new Metric.Builder("new_codepeer_check_violations", "New Codepeer check violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer check violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));
//
//    public static final Metric NEW_CODEPEER_WARNING_VIOLATIONS = new Metric.Builder("new_codepeer_warning_violations", "New Codepeer warning violations", Metric.ValueType.INT)
//                        .setDescription("New Codepeer warning violations")
//                        .setDirection(Metric.DIRECTION_WORST)
//                        .setQualitative(Boolean.TRUE)
//                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

   private static List<Metric> metrics = new ArrayList<Metric>();

   public List<Metric> getMetrics() {
    if (metrics.isEmpty()) {
      for (Field field : CodepeerMetrics.class.getFields()) {
        if (Metric.class.isAssignableFrom(field.getType())) {
          try {
              metrics.add((Metric) field.get(null));
          } catch (IllegalAccessException e) {
            throw new SonarException("cannot load metrics from " + CodepeerMetrics.class.getSimpleName(), e);
          }
        }
      }
    }
    return metrics;
  }
}
