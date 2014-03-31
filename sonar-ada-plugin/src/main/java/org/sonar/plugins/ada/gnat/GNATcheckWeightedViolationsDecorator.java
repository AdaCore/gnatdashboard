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

import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import com.google.common.collect.TreeMultiset;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependedUpon;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.*;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.utils.KeyValueFormat;
import org.sonar.plugins.ada.utils.AbstractAdaDecorator;

import java.util.Collection;
import java.util.Map;

/**
 * Computes GNATcheck metrics and weights them, ie. associates them with a
 * weight value given by their severity, and stores the measures.
 */
public class GNATcheckWeightedViolationsDecorator extends AbstractAdaDecorator {
  static private Map<RulePriority, Integer> weights;

  @DependsUpon
  public Metric input() {
    return GNATcheckMetrics.GNATCHECK_VIOLATIONS;
  }

  @DependedUpon
  public Metric output() {
    return GNATcheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS;
  }

  @Override
  public void decorate(Resource resource, DecoratorContext context) {
    double debt = 0;

    final Multiset<RulePriority> violations = TreeMultiset.create();
    final Collection<RuleMeasure> measures = context.getMeasures(
        MeasuresFilters.rules(GNATcheckMetrics.GNATCHECK_VIOLATIONS));

    for (RuleMeasure measure : measures) {
      if (MeasureUtils.hasValue(measure)) {
        final RulePriority severity = measure.getSeverity();
        final int count = measure.getIntValue();

        violations.add(severity, count);
        debt += getPriorityWeights().get(severity) * count;
      }
    }

    final Measure debtMeasure =
        new Measure(GNATcheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS, debt,
            KeyValueFormat.format(violations));

    if (debtMeasure.getValue() > 0) {
      context.saveMeasure(debtMeasure);
    }
  }

  /**
   * Returns a mapping between {@code RulePriority} and their default weight.
   * Lazily creates the hash map.
   *
   * @return The mapping.
   */
  static private Map<RulePriority, Integer> getPriorityWeights() {
    if (weights == null) {
      weights = Maps.newHashMap();

      // INFO=0;MINOR=1;MAJOR=3;CRITICAL=5;BLOCKER=10
      weights.put(RulePriority.INFO, 0);
      weights.put(RulePriority.MINOR, 1);
      weights.put(RulePriority.MAJOR, 3);
      weights.put(RulePriority.CRITICAL, 5);
      weights.put(RulePriority.BLOCKER, 10);
    }

    return weights;
  }

  @Override
  public String toString() {
    return "GNATcheck Decorator :: Compute Metrics Weight";
  }
}
