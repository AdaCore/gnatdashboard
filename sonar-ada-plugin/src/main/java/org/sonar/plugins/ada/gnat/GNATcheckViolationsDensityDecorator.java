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

import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependedUpon;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.utils.AbstractAdaDecorator;

import java.util.Arrays;
import java.util.List;

public class GNATcheckViolationsDensityDecorator extends AbstractAdaDecorator {
  /**
   * Density formula.
   *
   * @param debt The dept.
   * @param ncloc The number of relevant SLOC.
   * @return The density value.
   */
  protected static double calculate(int debt, int ncloc) {
    double rci = (1.0 - ((double) debt / (double) ncloc)) * 100.0;
    rci = Math.max(rci, 0.0);
    return rci;
  }

  @DependsUpon
  public List<Metric> input() {
    return Arrays.asList(
        GNATcheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS, CoreMetrics.NCLOC);
  }

  @DependedUpon
  public Metric output() {
    return GNATcheckMetrics.GNATCHECK_VIOLATIONS_DENSITY;
  }

  @Override
  public void decorate(Resource resource, DecoratorContext context) {
    if (shouldDecorateResource(resource)) {
      final Measure ncloc = context.getMeasure(CoreMetrics.NCLOC);

      if (MeasureUtils.hasValue(ncloc) && ncloc.getValue() > 0) {
        final Metric metric = GNATcheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS;
        final Measure debt = context.getMeasure(metric);
        int debtValue = 0;

        if (MeasureUtils.hasValue(debt)) {
          debtValue = debt.getIntValue();
        }

        if (!MeasureUtils.hasValue(context.getMeasure(metric))) {
          double density = calculate(debtValue, ncloc.getIntValue());
          context.saveMeasure(metric, density);
        }
      }
    }
  }

  @Override
  public String toString() {
    return "GNATcheck Decorator :: Compute Metric Violation Density";
  }
}
