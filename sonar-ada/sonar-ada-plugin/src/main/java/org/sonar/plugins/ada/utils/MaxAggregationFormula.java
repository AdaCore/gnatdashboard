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

package org.sonar.plugins.ada.utils;

import org.sonar.api.measures.*;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Deprecated
public class MaxAggregationFormula implements Formula {
  @Override
  public List<Metric> dependsUponMetrics() {
    return Collections.emptyList();
  }

  @Override
  public Measure calculate(FormulaData data, FormulaContext context) {
    double max = 0;
    final Collection<Measure> measures =
        data.getChildrenMeasures(context.getTargetMetric());

    for (final Measure measure : measures) {
      if (MeasureUtils.hasValue(measure)) {
        max = Math.max(max, measure.getIntValue());
      }
    }

    return new Measure(context.getTargetMetric(), max);
  }
}
