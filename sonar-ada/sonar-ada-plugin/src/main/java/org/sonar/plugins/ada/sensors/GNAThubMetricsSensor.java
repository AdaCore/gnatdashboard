/*
 * GNATdashboard
 * Copyright (C) 2016-2018, AdaCore
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

import com.adacore.gnatdashboard.gnathub.api.orm.FileMeasures;
import com.adacore.gnatdashboard.gnathub.api.orm.constant.GNATmetricMetrics;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.plugins.ada.GNAThub;
import org.sonar.plugins.ada.metrics.GNAThubMetrics;

import java.util.Optional;
import java.util.function.BiConsumer;

/**
 * Save file metrics from GNAThub to SonarQube.
 */
public class GNAThubMetricsSensor extends MainFilesSensor {
  @Override
  public String getName() {
    return "GNAThub Metrics Import";
  }

  @Override
  public void forInputFile(final SensorContext context, final GNAThub gnathub, final InputFile file) {
    final FileMeasures measures = gnathub.getMeasures().forFile(file.absolutePath());

    if (measures == null) {
      return;
    }
    final BiConsumer<String, Metric<Integer>> saveAsInt =
            (gnathubMetric, sonarMetric) ->
                    Optional.ofNullable(measures.asInt(gnathubMetric))
                            .ifPresent(value -> context.<Integer>newMeasure().on(file)
                                    .withValue(value).forMetric(sonarMetric).save());

    final BiConsumer<String, Metric<Double>> saveAsDouble =
            (gnathubMetric, sonarMetric) ->
                    Optional.ofNullable(measures.asDouble(gnathubMetric))
                            .ifPresent(value -> context.<Double>newMeasure().on(file)
                                    .withValue(value).forMetric(sonarMetric).save());

    saveAsInt.accept(GNATmetricMetrics.ALL_LINES, CoreMetrics.LINES);
    saveAsInt.accept(GNATmetricMetrics.COMMENT_LINES, CoreMetrics.COMMENT_LINES);
    saveAsInt.accept(GNATmetricMetrics.EOL_COMMENTS, GNAThubMetrics.EOL_COMMENTS);
    saveAsInt.accept(GNATmetricMetrics.BLANK_LINES, GNAThubMetrics.BLANK_LINES);
    saveAsDouble.accept(
            GNATmetricMetrics.COMMENT_PERCENTAGE, GNAThubMetrics.COMMENT_PERCENTAGE);

    //  Save cyclomatic complexity value as float in custom file complexity metric
    saveAsDouble.accept(
            GNATmetricMetrics.CYCLOMATIC_COMPLEXITY, GNAThubMetrics.FILE_CYCLOMATIC_COMPLEXITY);
    saveAsDouble.accept(
            GNATmetricMetrics.STATEMENT_COMPLEXITY, GNAThubMetrics.FILE_STATEMENT_COMPLEXITY);
    saveAsDouble.accept(
            GNATmetricMetrics.EXPRESSION_COMPLEXITY, GNAThubMetrics.FILE_EXPRESSION_COMPLEXITY);
    saveAsDouble.accept(
            GNATmetricMetrics.ESSENTIAL_COMPLEXITY, GNAThubMetrics.FILE_ESSENTIAL_COMPLEXITY);
    saveAsDouble.accept(
            GNATmetricMetrics.MAX_LOOP_NESTING, GNAThubMetrics.FILE_MAX_LOOP_NESTING);

    //  Save computed cyclomatic complexity value as integer in SonarQube complexity metric
    //  (the result is obtained by summing file methods cyclomatic complexity values)
    context.<Integer>newMeasure()
            .on(file)
            .forMetric(CoreMetrics.COMPLEXITY)
            .withValue(measures.complexity)
            .save();

    //  Save package level all_stmts metric as SonarQube LSLOC
    context.<Integer>newMeasure()
            .on(file)
            .forMetric(CoreMetrics.NCLOC)
            .withValue(measures.GNATmetricPckgAllStmts)
            .save();
  }
}
