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
import java.util.concurrent.TimeUnit;

@Slf4j
public abstract class MainFilesSensor implements Sensor {
  public abstract String getName();
  public abstract void forInputFile(
      final SensorContext context, final GNAThub gnathub, final InputFile file) throws SQLException;

  @Override
  public void describe(final SensorDescriptor descriptor) {
    descriptor.name(this.getName())
        .onlyOnLanguage(Ada.KEY)
        .onlyOnFileType(InputFile.Type.MAIN);
  }

  @SuppressWarnings("unused")
  public void setUp(final SensorContext context) {}

  @SuppressWarnings("unused")
  public void tearDown(final SensorContext context) {}

  @Override
  public void execute(final SensorContext context) {
    final GNAThub gnathub = new GNAThub(context.settings());
    final FileSystem fs = context.fileSystem();
    final FilePredicate mainFilePredicate = fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY));
    // Integration with SonarQube progress reporter.
    final ProgressReport progress = new ProgressReport(
        String.format("{} Reporter", getName()), TimeUnit.SECONDS.toMillis(10));
    progress.start(Lists.newArrayList(fs.files(mainFilePredicate)));
    setUp(context);

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

    tearDown(context);
    progress.stop();
  }
}
