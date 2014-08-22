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

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.DecoratorBarriers;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.MeasuresFilters;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.utils.AbstractAdaDecorator;
import org.sonar.plugins.ada.utils.CustomMetrics;
import org.sonar.plugins.ada.utils.GNAThubEncoding;

import java.util.Collection;

/**
 * Decorator for CodePeer violations metrics.
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
@Slf4j
@DependsUpon(DecoratorBarriers.ISSUES_TRACKED)
public class CodePeerViolationsDecorator extends AbstractAdaDecorator {

  @EqualsAndHashCode
  @AllArgsConstructor
  private class Entry {
    private final RulePriority severity;
    private final CodePeerCategory category;
  }

  private final Multiset<Entry> counts = HashMultiset.create();

  @Override
  public void decorate(Resource resource, DecoratorContext context) {
    if (shouldDecorateResource(resource)) {
      resetCounters();
      countViolations(context);
      saveTotalViolations(context);
      saveViolationsBySeverityAndCategory(context);
    }
  }

  /**
   * Resets variable and container that store temporary values.
   */
  private void resetCounters() {
    counts.clear();
  }

  /**
   * Saves CodePeer violations measure for the current resource.
   * Count the number of violations for each association of CodePeer
   * categories - severities.
   *
   * @param context To save the measure.
   */
  private void saveViolationsBySeverityAndCategory(DecoratorContext context) {
    for (final CodePeerCategory category : CodePeerCategory.values()) {
      for (final RulePriority severity : RulePriority.values()) {
        if (severity.equals(RulePriority.BLOCKER)) {
          // CodePeer has no BLOCKER level rule
          continue;
        }

        final Metric metric = CustomMetrics.getMetric(CodePeerMetrics.class,
            severity.toString(), category.toString());

        if (metric != null && context.getMeasure(metric) == null) {
          final Collection<Measure> measures =
              context.getChildrenMeasures(
                  MeasuresFilters.metric(metric.getKey()));
          final Double sum = MeasureUtils.sum(true, measures) +
              counts.count(new Entry(severity, category));

          context.saveMeasure(metric, sum);
        }
      }
    }
  }

  /**
   * Saves the total number of violations of all kind for the current resource.
   *
   * @param context To save the measure.
   */
  private void saveTotalViolations(DecoratorContext context) {
    final Metric cv = CodePeerMetrics.CODEPEER_VIOLATIONS;

    // Find violations measure for the resource
    if (context.getMeasure(cv) == null) {
      final Collection<Measure> violations = context.getChildrenMeasures(cv);

      // For source files sum of children = 0, for directories total = 0
      final Double sum = MeasureUtils.sum(true, violations) + counts.size();

      // Add the measure for the current resource
      context.saveMeasure(cv, sum);
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

      if (rule.getRepositoryKey().equals(CodePeerRuleRepository.KEY)) {
        // CodePeer rules are encoded as: SEVERITY__CATEGORY__RULE
        final String[] parts = GNAThubEncoding.fromSonarRuleKey(rule.getKey());

        try {
          final CodePeerCategory category = CodePeerCategory.valueOf(parts[1]);
          counts.add(new Entry(rule.getSeverity(), category));

        } catch (IllegalArgumentException why) {
          log.warn("{}: unknown category, skipping {}",
              new Object[]{parts[1], violation.getMessage()}, why);
        }
      }
    }
  }

  @Override
  public String toString() {
    return "CodePeer Decorator :: Compute CodePeer Metrics from Issues";
  }
}
