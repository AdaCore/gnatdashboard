/*
 * GNATdashboard
 * Copyright (C) 2016, AdaCore
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package org.sonar.plugins.ada;

import com.google.common.collect.ImmutableList;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.sonar.api.measures.*;
import org.sonar.plugins.ada.utils.MaxAggregationFormula;

import java.util.ArrayList;
import java.util.List;

/**
 * Register additional metrics to SonarQube.
 *
 * Defines a number of metrics computed by tools integrated with
 * GNATdashboard. Also provides a mapping to existing metrics as some of these
 * metrics are already defined by SonarQube's core engine.
 *
 * Once defined, these metrics can be assigned values during an analysis.
 */
@Deprecated
public class AdaMetrics implements Metrics {
  public static final String DOMAIN_CODING_STANDARD = "Coding standard";
  public static final String DOMAIN_STATIC_ANALYSIS = "Static analysis";

  public static final String GNATCHECK_KEY = "gnatcheck_total";
  public static final Metric<Integer> GNATCHECK =
      new Metric.Builder(
          GNATCHECK_KEY,
          "GNATcheck violations",
          Metric.ValueType.INT
      ).setDescription("Number of GNATcheck violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setBestValue(0.0)
          .setDomain(DOMAIN_CODING_STANDARD)
          .setFormula(new SumChildValuesFormula(false))
          .create();

  public static final String CODEPEER_KEY = "codepeer_total";
  public static final Metric<Integer> CODEPEER =
      new Metric.Builder(
          CODEPEER_KEY, "CodePeer messages", Metric.ValueType.INT
      ).setDescription("Number of CodePeer messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setBestValue(0.0)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .setFormula(new SumChildValuesFormula(false))
          .create();

    /*
     * Missing implementation of LSLOC at the GNAThub level. Currently, only
     * file-level metrics reported by GNATmetric are supported.
     */

    /*
    public static final String LSLOC_KEY = "lsloc";
    public static final Metric<Integer> LSLOC =
            new Metric.Builder(
                    LSLOC_KEY,
                    "Logical source line of code",
                    Metric.ValueType.INT
            ).setDescription("Number of logical source line of code")
             .setDirection(Metric.DIRECTION_NONE)
             .setQualitative(Boolean.FALSE)
             .setDomain(CoreMetrics.DOMAIN_SIZE)
             .setFormula(new SumChildValuesFormula(false))
             .create();
    */

  public static final String BLANK_LINE_KEY = "blank_lines";
  public static final Metric<Integer> BLANK_LINE =
      new Metric.Builder(
          BLANK_LINE_KEY,
          "Blank lines",
          Metric.ValueType.INT
      ).setDescription("Number of identified blank lines")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .setFormula(new SumChildValuesFormula(false))
          .create();

  public static final String EOL_COMMENT_KEY = "eol_comments";
  public static final Metric<Integer> EOL_COMMENT =
      new Metric.Builder(
          EOL_COMMENT_KEY,
          "End of line comments",
          Metric.ValueType.INT
      ).setDescription("Number of identified end of line comments")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_DOCUMENTATION)
          .setFormula(new SumChildValuesFormula(false))
          .create();

  public static final String STATEMENT_COMPLEXITY_KEY =
      "statement_complexity";
  public static final Metric<Double> STATEMENT_COMPLEXITY =
      new Metric.Builder(
          STATEMENT_COMPLEXITY_KEY,
          "Statement complexity",
          Metric.ValueType.FLOAT
      ).setDescription("Statements complexity file average")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .setFormula(new MeanAggregationFormula(true))
          .create();

  public static final String EXPRESSION_COMPLEXITY_KEY =
      "expression_complexity";
  public static final Metric<Double> EXPRESSION_COMPLEXITY =
      new Metric.Builder(
          EXPRESSION_COMPLEXITY_KEY,
          "Expression complexity",
          Metric.ValueType.FLOAT
      ).setDescription("Expression complexity")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .setFormula(new MeanAggregationFormula(true))
          .create();

  public static final String ESSENTIAL_COMPLEXITY_KEY =
      "essential_complexity";
  public static final Metric<Double> ESSENTIAL_COMPLEXITY =
      new Metric.Builder(
          ESSENTIAL_COMPLEXITY_KEY,
          "Essential complexity",
          Metric.ValueType.FLOAT
      ).setDescription("Essential complexity")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .setFormula(new MeanAggregationFormula(true))
          .create();

  public static final String MAX_LOOP_NESTING_KEY = "max_loop_nesting";
  public static final Metric<Integer> MAX_LOOP_NESTING =
      new Metric.Builder(
          MAX_LOOP_NESTING_KEY,
          "Maximum loop nesting",
          Metric.ValueType.INT
      ).setDescription("Maximum loop nesting")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .setFormula(new MaxAggregationFormula())
          .create();

  /**
   * Mapping to SonarQube metrics.
   */
  @AllArgsConstructor
  public enum Mapping {
    // Documentation
    COMMENT_LINES(CoreMetrics.COMMENT_LINES),
    COMMENT_PERCENTAGE(CoreMetrics.COMMENT_LINES_DENSITY),
    BLANK_LINES(AdaMetrics.BLANK_LINE),
    EOL_COMMENTS(AdaMetrics.EOL_COMMENT),

    // Code volumetry
    ALL_LINES(CoreMetrics.LINES),
    CODE_LINES(CoreMetrics.NCLOC),
    // LSLOC(AdaMetrics.LSLOC),

    // Code complexity
    CYCLOMATIC_COMPLEXITY(CoreMetrics.COMPLEXITY),
    STATEMENT_COMPLEXITY(AdaMetrics.STATEMENT_COMPLEXITY),
    EXPRESSION_COMPLEXITY(AdaMetrics.EXPRESSION_COMPLEXITY),
    ESSENTIAL_COMPLEXITY(AdaMetrics.ESSENTIAL_COMPLEXITY),
    MAX_LOOP_NESTING(AdaMetrics.MAX_LOOP_NESTING);

    @Getter
    private final Metric metric;
  }

  private static final List<Metric> METRICS;

  static {
    METRICS = new ArrayList<Metric>();
    METRICS.addAll(ImmutableList.of(
        GNATCHECK,
        CODEPEER,
        BLANK_LINE,
        EOL_COMMENT,
        // LSLOC,
        STATEMENT_COMPLEXITY,
        EXPRESSION_COMPLEXITY,
        ESSENTIAL_COMPLEXITY,
        MAX_LOOP_NESTING
    ));
  }

  @Override
  public List<Metric> getMetrics() {
    return METRICS;
  }
}
