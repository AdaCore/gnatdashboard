/****************************************************************************
 *                              Sonar Ada Plugin                            *
 *                                                                          *
 *                     Copyright (C) 2013-2014, AdaCore                     *
 *                                                                          *
 * This is free software;  you can redistribute it  and/or modify it  under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  This software is distributed in the hope  that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 ****************************************************************************/

package org.sonar.plugins.ada.gnat;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.MeanAggregationFormula;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.plugins.ada.utils.CustomMetrics;
import org.sonar.plugins.ada.utils.MaxAggregationFormula;

/**
 * Defines GNATmetric-specific metrics.
 */
public final class GNATmetricMetrics extends CustomMetrics {

  // General

  public static final Metric LSLOC =
      new Metric.Builder("lsoc", "Logical source line of code",
                         Metric.ValueType.INT)
          .setDescription("Logical source line of code")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create().setFormula(new SumChildValuesFormula(false));

  public static final Metric BLANK_LINE =
      new Metric.Builder("blank_lines", "Blank lines", Metric.ValueType.INT)
          .setDescription("Identified blank lines")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_SIZE)
          .create().setFormula(new SumChildValuesFormula(false));

  public static final Metric EOL_COMMENT =
      new Metric.Builder("eol_comments", "End of line comments",
                         Metric.ValueType.INT)
          .setDescription("Identified all end of line comments")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_DOCUMENTATION)
          .create().setFormula(new SumChildValuesFormula(false));

  // Complexity

  public static final Metric STATEMENT_COMPLEXITY =
      new Metric.Builder("statement_complexity", "Statement complexity",
                         Metric.ValueType.FLOAT)
          .setDescription("Average of all statements complexity in the file")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create().setFormula(new MeanAggregationFormula(true));

  public static final Metric EXPRESSION_COMPLEXITY =
      new Metric.Builder("expression_complexity",
                         "Expression complexity", Metric.ValueType.FLOAT)
          .setDescription("Expression complexity")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create().setFormula(new MeanAggregationFormula(true));

  public static final Metric ESSENTIAL_COMPLEXITY =
      new Metric.Builder("essential_complexity", "Essential complexity",
                         Metric.ValueType.FLOAT)
          .setDescription("Essential complexity")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create().setFormula(new MeanAggregationFormula(true));

  public static final Metric MAX_LOOP_NESTING =
      new Metric.Builder("max_loop_nesting", "Maximum loop nesting",
                         Metric.ValueType.INT)
          .setDescription("Maximum loop nesting")
          .setDirection(Metric.DIRECTION_NONE)
          .setQualitative(Boolean.FALSE)
          .setDomain(CoreMetrics.DOMAIN_COMPLEXITY)
          .create().setFormula(new MaxAggregationFormula());

  /**
   * Mapping to SonarQube metrics.
   */
  @AllArgsConstructor
  public enum Mapping {
    // Documentation
    COMMENT_LINES(CoreMetrics.COMMENT_LINES),
    COMMENT_PERCENTAGE(CoreMetrics.COMMENT_LINES_DENSITY),
    BLANK_LINES(GNATmetricMetrics.BLANK_LINE),
    EOL_COMMENTS(GNATmetricMetrics.EOL_COMMENT),

    // Size
    ALL_LINES(CoreMetrics.LINES),
    CODE_LINES(CoreMetrics.NCLOC),
    LSLOC(GNATmetricMetrics.LSLOC),

    // Complexity
    CYCLOMATIC_COMPLEXITY(CoreMetrics.COMPLEXITY),
    STATEMENT_COMPLEXITY(GNATmetricMetrics.STATEMENT_COMPLEXITY),
    EXPRESSION_COMPLEXITY(GNATmetricMetrics.EXPRESSION_COMPLEXITY),
    ESSENTIAL_COMPLEXITY(GNATmetricMetrics.ESSENTIAL_COMPLEXITY),
    MAX_LOOP_NESTING(GNATmetricMetrics.MAX_LOOP_NESTING);

    @Getter private final Metric metric;
  }
}
