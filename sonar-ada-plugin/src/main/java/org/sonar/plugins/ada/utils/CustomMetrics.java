/****************************************************************************
 *                              Sonar Ada Plugin                            *
 *                                                                          *
 *                        Copyright (C) 2014, AdaCore                       *
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

package org.sonar.plugins.ada.utils;

import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.utils.SonarException;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for custom metrics package. Automatically register all attributes
 * as a metric.
 */
public class CustomMetrics implements Metrics {
  private final List<Metric> metrics = new ArrayList<Metric>();

  public static final String DOMAIN_CODING_STANDARD = "Coding standard";
  public static final String DOMAIN_STATIC_ANALYSIS = "Static analysis";

  /**
   * Populates the internal {@code metrics} list.
   * Clears the list prior to population to avoid duplicates.
   */
  private void populateMetrics() {
    metrics.clear();

    for (Field field : this.getClass().getFields()) {
      if (Metric.class.isAssignableFrom(field.getType())) {
        try {
          metrics.add((Metric) field.get(null));
        } catch (IllegalAccessException e) {
          throw new SonarException("failed to load metrics", e);
        }
      }
    }
  }

  /**
   * Retrieves all metrics in this class by introspection.
   *
   * @return All the metrics declared as attribute.
   */
  @Override
  public List<Metric> getMetrics() {
    if (metrics.isEmpty()) {
      this.populateMetrics();
    }
    return metrics;
  }

  /**
   * Retrieves Metric for violations for a given repository, category and
   * severity. Only category can be null.
   *
   * @param klazz Must be derived from CustomMetrics.
   * @param severity
   * @param category
   * @return Metric
   */
  public static Metric getMetric(Class klazz, String severity, String category) {
    for (final Field field : klazz.getFields()) {
      if (!Metric.class.isAssignableFrom(field.getType())) {
        continue;
      }

      try {
        if ((category == null && field.getName().contains(severity)) ||
            (category != null && field.getName().contains(severity) &&
                field.getName().contains(category))) {

          return (Metric) field.get(null);
        }
      } catch (IllegalAccessException why) {
        throw new SonarException(
            "Cannot load metrics from " +  klazz.getSimpleName(), why);
      }
    }

    return null;
  }
}
