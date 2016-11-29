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
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;

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
public class CountMetrics implements Metrics {
  private static final String DOMAIN_TOTALS = "Totals";

  public static final Metric<Integer> GNATCHECK =
      new Metric.Builder(
          "gnatcheck_total",
          "Total GNATcheck violations",
          Metric.ValueType.INT
      ).setDescription("Number of GNATcheck violations")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setBestValue(0.)
          .setDomain(DOMAIN_TOTALS)
          .create();

  public static final Metric<Integer> CODEPEER =
      new Metric.Builder(
          "codepeer_total",
          "Total CodePeer messages",
          Metric.ValueType.INT
      ).setDescription("Number of CodePeer messages")
          .setDirection(Metric.DIRECTION_WORST)
          .setQualitative(true)
          .setBestValue(0.)
          .setDomain(DOMAIN_TOTALS)
          .create();

  @Override
  public List<Metric> getMetrics() {
    return ImmutableList.of(GNATCHECK, CODEPEER);
  }
}
