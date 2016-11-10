/*
 * Sonar Ada Plugin (GNATdashboard)
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

package org.sonar.plugins.ada.gnat;

import lombok.AllArgsConstructor;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.AdaMetrics;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.persistence.MeasureRecord;
import org.sonar.plugins.ada.persistence.ProjectDAO;
import org.sonar.plugins.ada.utils.ResourceUtils;

import java.util.Collection;

@Slf4j
@ToString
@AllArgsConstructor
public class GNATmetricSensor implements Sensor {
  private static final String NAME = "GNATmetric";

  private final FileSystem fs;
  private final AdaProjectContext adaContext;

  @Override
  public boolean shouldExecuteOnProject(final Project project) {
    return fs.hasFiles(fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY)));
  }

  @Override
  public void analyse(final Project project, final SensorContext context) {
    log.info("Loading code metrics from GNAThub (provided by GNATmetric)");

    if (!adaContext.isDAOLoaded()) {
      log.error("GNAThub db not loaded, cannot list GNATmetric measures");
      return;
    }

    final ProjectDAO dao = adaContext.getDao();
    final Collection<Resource> scope =
        ResourceUtils.expandChildren(project, context);

    for (final MeasureRecord am : dao.getMeasuresByTool(NAME)) {
      // Check that the resource is from the correct project
      // ??? Augment SQL query to filter out resources per project
      final File file = am.getFile();
      if (!scope.contains(file)) {
        log.trace("{}: not from project: {}",
            file.getLongName(), project.getName());
        continue;
      }

      log.debug("({}) {} = {}", new Object[]{
          am.getPath(),
          am.getMeasure().getMetricKey(),
          am.getMeasure().getValue()
      });

      final Metric metric = AdaMetrics.Mapping.valueOf(
          am.getMeasure().getMetricKey().toUpperCase()).getMetric();
      context.saveMeasure(file,
          new Measure(metric, am.getMeasure().getValue()));
    }
  }
}
