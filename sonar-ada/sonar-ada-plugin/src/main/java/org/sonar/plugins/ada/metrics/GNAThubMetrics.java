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

package org.sonar.plugins.ada.metrics;

import com.google.common.collect.ImmutableList;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;

import java.util.List;

/**
 * Register additional metrics to SonarQube.
 *
 * Defines a number of metrics computed by tools integrated with GNATdashboard.
 * Once defined, these metrics can be assigned values during an analysis.
 */
public final class GNAThubMetrics implements Metrics {
  /*
   * NOTE: missing implementation of LSLOC at the GNAThub level.
   * Currently, only file-level metrics reported by GNATmetric are supported.
   *
   * TODO(delay): register this metric with SonarQube once GNAThub can report it (see P511-010).
   */
  @SuppressWarnings("unused")
  public static final Metric<Integer> LSLOC =
      new Metric.Builder(
          "gnatmetric_lsloc",
          "Logical source line of code",
          Metric.ValueType.INT
      ).setDescription("Number of logical source line of code")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create();

  public static final Metric<Integer> BLANK_LINES =
      new Metric.Builder(
          "blank_lines",
          "Blank lines",
          Metric.ValueType.INT
      ).setDescription("Number of identified blank lines")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(false)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create();

  public static final Metric<Integer> EOL_COMMENTS =
      new Metric.Builder(
          "eol_comments",
          "End of line comments",
          Metric.ValueType.INT
      ).setDescription("Number of lines containing end-of-line comments")
          .setDirection(Metric.DIRECTION_BETTER)
          .setQualitative(false)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create();

  public static final Metric<Double> COMMENT_PERCENTAGE =
      new Metric.Builder(
          "gnatmetric_comment_percentage",
          "Comment density / File",
          Metric.ValueType.FLOAT
      ).setDescription("Comment density per file")
          .setDirection(Metric.DIRECTION_BETTER)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create();

  public static final Metric<Double> FILE_CYCLOMATIC_COMPLEXITY =
      new Metric.Builder(
          "gnatmetric_file_cyclomatic_complexity",
          "Cyclomatic complexity / File",
          Metric.ValueType.FLOAT
      ).setDescription("Average cyclomatic complexity per file")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create();

  public static final Metric<Double> FILE_STATEMENT_COMPLEXITY =
      new Metric.Builder(
          "gnatmetric_file_statement_complexity",
          "Statement complexity / File",
          Metric.ValueType.FLOAT
      ).setDescription("Average statements complexity per file")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create();

  public static final Metric<Double> FILE_EXPRESSION_COMPLEXITY =
      new Metric.Builder(
          "gnatmetric_file_expression_complexity",
          "Expression complexity / File",
          Metric.ValueType.FLOAT
      ).setDescription("Average expression complexity per file")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create();

  public static final Metric<Double> FILE_ESSENTIAL_COMPLEXITY =
      new Metric.Builder(
          "gnatmetric_file_essential_complexity",
          "Essential complexity / File",
          Metric.ValueType.FLOAT
      ).setDescription("Average essential complexity per file")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create();

  public static final Metric<Double> FILE_MAX_LOOP_NESTING =
      new Metric.Builder(
          "gnatmetric_file_max_loop_nesting",
          "Maximum loop nesting / File",
          Metric.ValueType.FLOAT
      ).setDescription("Average MAX_LOOP_NESTING per file")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create();

  @Override
  public List<Metric> getMetrics() {
    return ImmutableList.of(
        BLANK_LINES,
        EOL_COMMENTS,
        COMMENT_PERCENTAGE,
        FILE_CYCLOMATIC_COMPLEXITY,
        FILE_STATEMENT_COMPLEXITY,
        FILE_EXPRESSION_COMPLEXITY,
        FILE_ESSENTIAL_COMPLEXITY,
        FILE_MAX_LOOP_NESTING
    );
  }
}
