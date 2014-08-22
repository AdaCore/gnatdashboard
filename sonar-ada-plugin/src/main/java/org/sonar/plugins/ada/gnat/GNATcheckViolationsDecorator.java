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

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import org.sonar.api.batch.DecoratorBarriers;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.*;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.utils.AbstractAdaDecorator;
import org.sonar.plugins.ada.utils.CustomMetrics;

import java.util.Collection;
import java.util.Map;

/**
 * Decorator for GNATcheck violations metrics.
 *
 * Uses the same computation algorithm than in
 * {@code org.sonar.plugins.core.sensors.ViolationsDecorator}.
 *
 * This decorator is necessary since GNATcheck and CodePeer violations are
 * separated, as there are processed as violations but the semantic of
 * the information given by those tool are different; so it is needed
 * to be able to distinguish them in order to represent it differently
 * in the GUI.
 */
@DependsUpon(DecoratorBarriers.ISSUES_TRACKED)
public class GNATcheckViolationsDecorator extends AbstractAdaDecorator {

  private final Multiset<Rule> rules = HashMultiset.create();
  private final Multiset<RulePriority> severities = HashMultiset.create();
  private final Map<Rule, RulePriority> ruleToSeverity = Maps.newHashMap();

  @Override
  public void decorate(Resource resource, DecoratorContext context) {
    if (shouldDecorateResource(resource)) {
      resetCounters();
      countViolations(context);
      saveTotalViolations(context);
      saveViolationsByRule(context);
      saveViolationsBySeverity(context);
    }
  }

  /**
   * Resets variable and container that store temporary values.
   */
  private void resetCounters() {
    rules.clear();
    severities.clear();
    ruleToSeverity.clear();
  }

  /**
   * Saves GNATcheck violations measure for the current resource.
   * Count the number of violations for each severity.
   *
   * @param context To save the measure.
   */
  private void saveViolationsBySeverity(DecoratorContext context) {
    for (final RulePriority severity : RulePriority.values()) {
      final Metric metric = CustomMetrics.getMetric(GNATcheckMetrics.class,
          severity.toString(), null /* category */);

      if (metric != null && context.getMeasure(metric) == null) {
        final Collection<Measure> children =
            context.getChildrenMeasures(
                MeasuresFilters.metric(metric.getKey()));
        final Double sum = MeasureUtils.sum(true, children) +
            severities.count(severity);

        context.saveMeasure(metric, sum);
      }
    }
  }

  /**
   * Saves GNATcheck violations measure for the current resource.
   * Count the number of violations for each rule.
   *
   * @param context To save the measure.
   */
  private void saveViolationsByRule(DecoratorContext context) {
    final Metric metric = GNATcheckMetrics.GNATCHECK_VIOLATIONS;
    final Collection<Measure> measures =
        context.getChildrenMeasures(MeasuresFilters.rules(metric));

    for (final Measure measure : measures) {
      final RuleMeasure rm = (RuleMeasure) measure;
      final Rule rule = rm.getRule();

      if (rule != null && MeasureUtils.hasValue(rm)) {
        rules.add(rule, rm.getValue().intValue());
        ruleToSeverity.put(rm.getRule(), rm.getSeverity());
      }
    }

    for (final Multiset.Entry<Rule> entry : rules.entrySet()) {
      final Rule rule = entry.getElement();
      final RuleMeasure measure =
          RuleMeasure.createForRule(metric, rule, (double) entry.getCount());

      measure.setSeverity(ruleToSeverity.get(rule));
      context.saveMeasure(measure);
    }
  }

  /**
   * Saves the total number of violations of all kind for the current resource.
   *
   * @param context To save the measure.
   */
  private void saveTotalViolations(DecoratorContext context) {
    final Metric gcv = GNATcheckMetrics.GNATCHECK_VIOLATIONS;

    // Finds violations measure for the resource
    if (context.getMeasure(gcv) == null) {
      final Collection<Measure> violations = context.getChildrenMeasures(gcv);

      // For source files sum of children = 0, for directories total = 0
      final Double sum = MeasureUtils.sum(true, violations) + severities.size();

      // Add the measure for the current resource
      context.saveMeasure(gcv, sum);
    }
  }

  /**
   * Initializes counters.
   *
   * @param context To retrieve all the violations saved for the current
   *    resource.
   */
  private void countViolations(DecoratorContext context) {
    for (final Violation violation : context.getViolations()) {
      final Rule rule = violation.getRule();

      if (rule.getRepositoryKey().equals(GNATcheckRuleRepository.KEY)) {
        rules.add(violation.getRule());
        severities.add(rule.getSeverity());
        ruleToSeverity.put(rule, violation.getSeverity());
      }
    }
  }

  @Override
  public String toString() {
    return "GNATcheck Decorator :: Compute GNATcheck Metrics from Issues";
  }
}
