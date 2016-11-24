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

package org.sonar.plugins.ada.sensors;

import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.coverage.CoverageType;
import org.sonar.api.batch.sensor.coverage.NewCoverage;
import org.sonar.plugins.ada.GNAThub;

import java.util.Optional;

/**
 * Save coverage information from GNAThub into SonarQube.
 */
public class GNAThubCoverageSensor extends MainFilesSensor {
  @Override
  public String getName() {
    return "GNAThub Coverage Import";
  }

  @Override
  public void forInputFile(final SensorContext context, final GNAThub gnathub, final InputFile file)
  {
    // Collect and save the input file coverage information
    final NewCoverage newCoverage = context.newCoverage().onFile(file).ofType(CoverageType.UNIT);
    Optional.ofNullable(gnathub.getCoverage().forFile(file.absolutePath()))
        .ifPresent(coverage -> coverage.hits
            .forEach(hits -> newCoverage.lineHits(hits.line, hits.count)));
    newCoverage.save();
  }
}
