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

package org.sonar.plugins.ada.codepeer;

import org.sonar.api.measures.Metric;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.plugins.ada.utils.CustomMetrics;

/**
 * Defines CodePeer-specific metrics.
 *
 * Those metrics has been added in order to separate GNATcheck violations from
 *
 * CodePeer violations. Both are handled as issues in sonar processing, but
 * for the user:
 *  - GNATcheck information correspond to violations of coding standard
 *  - CodePeer information correspond to messages that point to potential bugs
 *
 * CodePeer violation are categorized by severity and category.
 *
 * Categories are:
 *  - check
 *  - warning
 *  - race condition
 *  - informational
 *
 * Severities are reported as: high, medium & low.
 */
public class CodePeerMetrics extends CustomMetrics {

  // Total of all CodePeer violations
  @SuppressWarnings("unused")
  public static final Metric CODEPEER_VIOLATIONS =
      new Metric.Builder("codepeer_violations",
                         "CodePeer messages", Metric.ValueType.INT)
          .setDescription("CodePeer messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  // Informational - Special category, might be removed in the future

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_INFORMATIONAL_INFO_VIOLATIONS =
      new Metric.Builder("codepeer_informational_info_violations",
                         "CodePeer informational messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer informational messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  // Critical

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_CHECK_CRITICAL_VIOLATIONS =
      new Metric.Builder("codepeer_check_critical_violations",
                         "CodePeer check high messages", Metric.ValueType.INT)
          .setDescription("CodePeer check high messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_WARNING_CRITICAL_VIOLATIONS =
      new Metric.Builder("codepeer_warning_critical_violations",
                         "CodePeer warning high messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer warning high messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_RACE_CONDITION_CRITICAL_VIOLATIONS =
      new Metric.Builder("codepeer_race_condition_critical_violations",
                         "CodePeer race condition high messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer race condition high messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  // Major

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_CHECK_MAJOR_VIOLATIONS =
      new Metric.Builder("codepeer_check_major_violations",
                         "CodePeer check medium messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer check medium messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_WARNING_MAJOR_VIOLATIONS =
      new Metric.Builder("codepeer_warning_major_violations",
                         "CodePeer warning medium messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer warning medium messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_RACE_CONDITION_MAJOR_VIOLATIONS =
      new Metric.Builder("codepeer_race_condition_major_violations",
                         "CodePeer race condition medium messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer race condition medium messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  // Minor

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_CHECK_MINOR_VIOLATIONS =
      new Metric.Builder("codepeer_check_minor_violations",
                         "CodePeer check low messages", Metric.ValueType.INT)
          .setDescription("CodePeer check low messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_WARNING_MINOR_VIOLATIONS =
      new Metric.Builder("codepeer_warning_minor_violations",
                         "CodePeer warning low messages",
                          Metric.ValueType.INT)
          .setDescription("CodePeer warning low messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric CODEPEER_RACE_CONDITION_MINOR_VIOLATIONS =
      new Metric.Builder("codepeer_race_condition_minor_violations",
                         "CodePeer race condition low messages",
                         Metric.ValueType.INT)
          .setDescription("CodePeer race condition low messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_STATIC_ANALYSIS)
          .create().setFormula(new SumChildValuesFormula(false));
}
