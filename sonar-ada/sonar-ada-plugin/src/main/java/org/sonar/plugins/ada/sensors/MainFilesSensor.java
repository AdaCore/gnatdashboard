/*
 * GNATdashboard
 * Copyright (C) 2016-2020, AdaCore
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

import com.adacore.gnatdashboard.gnathub.api.orm.Connector;
import com.google.common.collect.Lists;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.fs.FilePredicate;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.Sensor;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.SensorDescriptor;
import org.sonar.plugins.ada.GNAThub;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.squidbridge.ProgressReport;

import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.io.File;

@Slf4j
abstract class MainFilesSensor implements Sensor {
  protected abstract String getName();
  protected abstract void forInputFile(
      final SensorContext context, final GNAThub gnathub, final InputFile file);

  @Override
  public void describe(final SensorDescriptor descriptor) {
    descriptor.name(this.getName())
        .onlyOnLanguage(Ada.KEY)
        .onlyOnFileType(InputFile.Type.MAIN);
  }

  protected void tearDown() {}

  @Override
  public void execute(final SensorContext context) {
    if (!GNAThub.isGNAThubDBDefined(context.config())) {
      log.error("   Sonar Ada plugin: no results to import.");
      log.error("   Ada-like (*.ads and *.adb) source files detected but");
      log.error("   sonar.ada.gnathub.db is not defined in the sonar-project.properties file");
      log.error("   Use GNAThub to analyse and upload results for Ada projects.");
      log.error("   Sonar Ada plugin: import is stopped.");
      log.error("   ------------------------------------ ");

      // No GNAThub database is defined in sonar-project.properties file. Exiting!
      return;
    }

    final GNAThub gnathub = new GNAThub(context.config());
    final FileSystem fs = context.fileSystem();
    final FilePredicate mainFilePredicate = fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY));

    // Integration with SonarQube progress reporter.
    final ProgressReport progress = new ProgressReport(
        String.format("%s Reporter", getName()), TimeUnit.SECONDS.toMillis(10));

    List<InputFile> files = Lists.newArrayList(fs.inputFiles(mainFilePredicate));
    progress.start(files.stream().map(InputFile::file).collect(Collectors.toList()));

    try {
      @Cleanup("closeConnection") final Connector connector = gnathub.getConnector();
      connector.openConnection();

      for (final InputFile file : fs.inputFiles(mainFilePredicate)) {
        progress.nextFile();  // Notify progress reporter of a new step.
        this.forInputFile(context, gnathub, file);
      }

    } catch (final SQLException why) {
      log.error("Fatal SQL error", why);
    }

    tearDown();
    progress.stop();
  }
}
