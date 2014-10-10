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

package org.sonar.plugins.ada.gnu;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoverageMeasuresBuilder;
import org.sonar.api.measures.Measure;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.persistence.CoverageRecord;
import org.sonar.plugins.ada.utils.AbstractAdaSensor;

@Slf4j
@AllArgsConstructor
public class GcovSensor extends AbstractAdaSensor {
  private AdaProjectContext adaContext;

  @Override
  public void analyse(Project project, SensorContext context) {
    log.info("Collecting Gcov measures");

    if (!adaContext.isDAOLoaded()) {
      log.error("GNAThub db not loaded, cannot fetch Gcov measures");
      return;
    }

    File currentFile = null;
    CoverageMeasuresBuilder builder = CoverageMeasuresBuilder.create();

    // Retrieve all coverage data ordered by resource
    for (CoverageRecord data : adaContext.getDao().getCoverageByTool("GCov")) {
      if (currentFile == null) {
        currentFile = data.getFile();
      }

      // When switching file:
      //   - save measures for current file,
      //   - create a new builder for measure,
      //   - set current file to the next file
      if (!currentFile.equals(data.getFile())) {
        saveCoverageMeasure(builder, context, currentFile);
        builder = CoverageMeasuresBuilder.create();
        currentFile = data.getFile();
      }

      // Add hits for line
      builder.setHits(data.getLine(), data.getHits());
    }

    if (currentFile != null) {
      // For the last resource
      saveCoverageMeasure(builder, context, currentFile);
    }
  }

  private void saveCoverageMeasure(CoverageMeasuresBuilder builder,
                                   SensorContext context, File res) {
    for (Measure m : builder.createMeasures()) {
      context.saveMeasure(res, m);
    }
  }
}
