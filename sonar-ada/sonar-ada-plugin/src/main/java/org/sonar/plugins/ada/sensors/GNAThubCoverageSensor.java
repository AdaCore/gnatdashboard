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

package org.sonar.plugins.ada.sensors;

import com.google.common.collect.Lists;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.api.batch.sensor.coverage.CoverageType;
import org.sonar.api.batch.sensor.coverage.NewCoverage;
import org.sonar.plugins.ada.GNAThub;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.squidbridge.ProgressReport;

import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Save coverage information from GNAThub into SonarQube.
 */
public class GNAThubCoverageSensor implements Sensor {
  @Override
  public void describe(final SensorDescriptor descriptor) {
    descriptor.name("GNAThub Coverage Sensor")
        .onlyOnLanguage(Ada.KEY)
        .onlyOnFileType(InputFile.Type.MAIN);
  }

  @Override
  public void execute(final SensorContext context) {
    final GNAThub gnathub = new GNAThub(context.settings());
    final FileSystem fs = context.fileSystem();
    final FilePredicate mainFilePredicate = fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY));
    // Integration with SonarQube progress reporter
    final ProgressReport progress = new ProgressReport(
        "GNAThub Coverage Sensor Report", TimeUnit.SECONDS.toMillis(10));
    progress.start(Lists.newArrayList(fs.files(mainFilePredicate)));

    // Iterate over main files known to the SonarQube Scanner and save coverage for each
    for (final InputFile file : fs.inputFiles(mainFilePredicate)) {
      progress.nextFile(); // Notify progress reporter of a new step

      // Collect and save the input file coverage information
      final NewCoverage newCoverage = context.newCoverage().onFile(file).ofType(CoverageType.UNIT);
      Optional.ofNullable(gnathub.getCoverage().forFile(file.absolutePath()))
          .ifPresent(coverage -> coverage.getHits()
              .forEach(hits -> newCoverage.lineHits(hits.getLine(), hits.getCount())));
      newCoverage.save();
    }

    progress.stop();
  }
}
