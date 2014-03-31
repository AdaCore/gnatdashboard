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

import org.sonar.api.measures.Metric;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.plugins.ada.utils.CustomMetrics;

/**
 * Defines GNATcheck-specific metrics.
 */
public class GNATcheckMetrics extends CustomMetrics {

  // Total of all GNATcheck violations
  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_VIOLATIONS =
      new Metric.Builder("gnatcheck_violations",
                         "GNATcheck violations", Metric.ValueType.INT)
          .setDescription("GNATcheck violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_BLOCKER_VIOLATIONS =
      new Metric.Builder("gnatcheck_blocker_violations",
                         "GNATcheck blocker violations", Metric.ValueType.INT)
          .setDescription("GNATcheck blocker violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_CRITICAL_VIOLATIONS =
      new Metric.Builder("gnatcheck_critical_violations",
                         "GNATcheck critical violations", Metric.ValueType.INT)
          .setDescription("GNATcheck critical violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_MAJOR_VIOLATIONS =
      new Metric.Builder("gnatcheck_major_violations",
                         "GNATcheck major violations", Metric.ValueType.INT)
          .setDescription("GNATcheck major violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_MINOR_VIOLATIONS =
      new Metric.Builder("gnatcheck_minor_violations",
                         "GNATcheck minor violations", Metric.ValueType.INT)
          .setDescription("GNATcheck minor violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_INFO_VIOLATIONS =
      new Metric.Builder("gnatcheck_info_violations",
                         "GNATcheck informational violations",
                         Metric.ValueType.INT)
          .setDescription("GNATcheck informational violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD)
          .create().setFormula(new SumChildValuesFormula(false));

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_VIOLATIONS_DENSITY =
      new Metric.Builder("gnatcheck_violations_density",
                         "Coding standard compliance", Metric.ValueType.PERCENT)
          .setDescription("Coding standard compliance")
          .setDirection(Metric.DIRECTION_BETTER)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD).create();

  @SuppressWarnings("unused")
  public static final Metric GNATCHECK_WEIGHTED_VIOLATIONS =
      new Metric.Builder("gnatcheck_weighted_violations",
                         "GNATcheck weighted violations", Metric.ValueType.INT)
          .setHidden(true)
          .setDescription("GNATcheck weighted violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(Boolean.TRUE)
          .setDomain(DOMAIN_CODING_STANDARD).create();
}
