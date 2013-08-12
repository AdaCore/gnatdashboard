/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gcov;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoverageMeasuresBuilder;
import org.sonar.api.measures.Measure;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.api.resource.AdaFile;
import org.sonar.plugins.ada.persistence.AdaDao;
import org.sonar.plugins.ada.utils.AdaSensor;

public class GcovSensor extends AdaSensor {

    private final Logger log = LoggerFactory.getLogger(GcovSensor.class);

    @Override
    public void analyse(Project project, SensorContext context) {
        log.info("--- GCov sensor");

        AdaDao dao = new AdaDao();
        AdaFile currentFile = null;
        CoverageMeasuresBuilder builder = CoverageMeasuresBuilder.create();

        // Retrieve all coverage data ordered by resource
        for (CoverageData data : dao.getCoverageData()) {

            // Initialization of the current file var
            if (currentFile == null) {
                currentFile = data.getResource();
            }

            // When switching file:
            //   - save measures for current file,
            //   - create a new builder for measure,
            //   - set current file to the next file
            if (!currentFile.equals(data.getResource())) {
                saveCoverageMeasure(builder, context, currentFile);
                builder = CoverageMeasuresBuilder.create();
                currentFile = data.getResource();
            }

            // Add hits for line
            builder.setHits(data.getLine(), data.getHits());
        }
        if (currentFile != null) {
            // For the last resource
            saveCoverageMeasure(builder, context, currentFile);
        }
    }

    private void saveCoverageMeasure(CoverageMeasuresBuilder builder, SensorContext context, AdaFile res) {
        for (Measure m : builder.createMeasures()) {
            context.saveMeasure(res, m);
        }
    }
}
