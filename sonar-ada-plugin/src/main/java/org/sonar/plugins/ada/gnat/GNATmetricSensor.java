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
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.persistence.MeasureRecord;
import org.sonar.plugins.ada.utils.AbstractAdaSensor;

/**
 * Sensor for GNATmetric external tool, retrieve information from a report and
 * save the measures.
 */
@Slf4j
@AllArgsConstructor
public class GNATmetricSensor extends AbstractAdaSensor {
  private static final String NAME = "GNATmetric";

  private AdaProjectContext adaContext;

  @Override
  public void analyse(Project project, SensorContext context) {
    log.info("Collecting GNATmetric measures");

    if (!adaContext.isDAOLoaded()) {
      log.error("GNAThub db not loaded, cannot fetch GNATmetric measures");
      return;
    }

    for (final MeasureRecord am : adaContext.getDao().getMeasuresByTool(NAME)) {
      final File file = am.getFile();
      log.trace("Saving measure '{}' for file: {}",
          am.getMeasure().getMetricKey(), file.getLongName());

      final Metric metric = GNATmetricMetrics.Mapping.valueOf(
          am.getMeasure().getMetricKey().toUpperCase()).getMetric();
      final Measure measure = new Measure(metric, am.getMeasure().getValue());

      context.saveMeasure(file, measure);
    }
  }
}
