/*
 * Sonar Modeling Plugin
 * Copyright (C) 2013, AdaCore
 */

package com.adacore.sonar.reference;

import org.sonar.api.measures.AverageFormula;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;

import java.util.Arrays;
import java.util.List;

public final class ModelingMetrics implements Metrics {

  static String DOMAIN = "Traceability";

  public static final Metric EXP_REQS = new Metric.Builder("exp_reqs", "expected requirements", Metric.ValueType.INT)
      .setDescription("the number of requiremets a project manager expects his team to develop. " +
              "This number may be calculated based on some specific input (number of subprograms, " +
              "number of Simulink blocks, ...) or estimated.")
      .setDirection(Metric.DIRECTION_NONE)
      .setQualitative(true)
      .setDomain(DOMAIN)
      .setFormula(new SumChildValuesFormula (false))
      .create();

  public static final Metric DEV_REQS = new Metric.Builder("dev_reqs", "developed requirements", Metric.ValueType.PERCENT)
      .setDescription("the percentage of requirements already developed")
      .setBestValue(100.0)
      .setDirection(Metric.DIRECTION_BETTER)
      .setQualitative(false)
      .setDomain(DOMAIN)
      .create();

  public static final Metric UNTRACEABLE_SUBS = new Metric.Builder("untraceable_subs", "untraceable_subs", Metric.ValueType.PERCENT)
      .setDescription("the percentage of requirements already developed")
      .setBestValue(0.0)
      .setDirection(Metric.DIRECTION_WORST)
      .setQualitative(false)
      .setDomain(DOMAIN)
      .create();

  // getMetrics() method is defined in the Metrics interface and is used by
  // Sonar to retrieve the list of new metrics
  public List<Metric> getMetrics() {
    return Arrays.asList(EXP_REQS, DEV_REQS, UNTRACEABLE_SUBS);
  }
}
