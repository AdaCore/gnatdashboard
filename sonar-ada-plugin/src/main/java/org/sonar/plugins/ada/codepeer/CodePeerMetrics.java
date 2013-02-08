/*
 * Sonar Ada Plugin
 * Copyright (C) 2012-2013, AdaCore
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
 * Defines metrics specific linked to CodePeer.
 *
 * Those metrics has been added in order to separate GNATcheck violations from
 * CodePeer violations. Both are handle as violation in sonar processing, but
 * for the user:
 *  - GNATcheck information correspond to violations of coding  standard,
 *  - CodePeer information correspond to messages that point potential bugs,
 *    etc.
 * CodePeer violation are categorized by severity and category.
 * Categories are:
 *  - check
 *  - warning
 *  - race condition
 *  - informational
 * Severities are reported as:
 *  - high
 *  - medium
 *  - low
 *
 */
public class CodePeerMetrics implements Metrics{

    //Instance used by the plugin
    public static final CodePeerMetrics INSTANCE = new CodePeerMetrics();

    // Domain of CodePeer violations
    public static final String DOMAIN_STATIC_ANALYSIS = "Static analysis";


    // Total of all CodePeer violations
    public static final Metric CODEPEER_VIOLATIONS = new Metric.Builder("codepeer_violations", "CodePeer messages", Metric.ValueType.INT)
                        .setDescription("CodePeer messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    //-------------------------------------------------------------------
    //  INFORMATIONAL - Special category, might be removed in the future
    //-------------------------------------------------------------------
    public static final Metric CODEPEER_INFORMATIONAL_INFO_VIOLATIONS = new Metric.Builder("codepeer_informational_info_violations", "CodePeer informational messages", Metric.ValueType.INT)
                        .setDescription("CodePeer informational messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    //-----------------
    //    CRITICAL
    //-----------------
    public static final Metric CODEPEER_CHECK_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_check_critical_violations", "CodePeer check high messages", Metric.ValueType.INT)
                        .setDescription("CodePeer check high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_warning_critical_violations", "CodePeer warning high messages", Metric.ValueType.INT)
                        .setDescription("CodePeer warning high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_CRITICAL_VIOLATIONS = new Metric.Builder("codepeer_race_condition_critical_violations", "CodePeer race condition high messages", Metric.ValueType.INT)
                        .setDescription("CodePeer race condition high messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    //-----------------
    //      MAJOR
    //-----------------
    public static final Metric CODEPEER_CHECK_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_check_major_violations", "CodePeer check medium messages", Metric.ValueType.INT)
                        .setDescription("CodePeer check medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_warning_major_violations", "CodePeer warning medium messages", Metric.ValueType.INT)
                        .setDescription("CodePeer warning medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_MAJOR_VIOLATIONS = new Metric.Builder("codepeer_race_condition_major_violations", "CodePeer race condition medium messages", Metric.ValueType.INT)
                        .setDescription("CodePeer race condition medium messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    //-----------------
    //      MINOR
    //-----------------
    public static final Metric CODEPEER_CHECK_MINOR_VIOLATIONS = new Metric.Builder("codepeer_check_minor_violations", "CodePeer check low messages", Metric.ValueType.INT)
                        .setDescription("CodePeer check low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_WARNING_MINOR_VIOLATIONS = new Metric.Builder("codepeer_warning_minor_violations", "CodePeer warning low messages", Metric.ValueType.INT)
                        .setDescription("CodePeer warning low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CODEPEER_RACE_CONDITION_MINOR_VIOLATIONS = new Metric.Builder("codepeer_race_condition_minor_violations", "CodePeer race condition low messages", Metric.ValueType.INT)
                        .setDescription("CodePeer race condition low messages")
                        .setDirection(Metric.DIRECTION_WORST)
                        .setQualitative(Boolean.TRUE)
                        .setDomain(DOMAIN_STATIC_ANALYSIS).create().setFormula(new SumChildValuesFormula(false));

   // All CodePeer Metrics
   private static List<Metric> metrics = new ArrayList<Metric>();

   /**
    * Retrieves by introspection all CodePeer metrics.
    * @return all the CodePeer metrics declared as attribute
    */
   public List<Metric> getMetrics() {
    if (metrics.isEmpty()) {
      for (Field field : CodePeerMetrics.class.getFields()) {
        if (Metric.class.isAssignableFrom(field.getType())) {
          try {
              metrics.add((Metric) field.get(null));
          } catch (IllegalAccessException e) {
            throw new SonarException("cannot load metrics from " + CodePeerMetrics.class.getSimpleName(), e);
          }
        }
      }
    }
    return metrics;
  }
}
