/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2013-2015, AdaCore
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

package org.sonar.plugins.ada.coverage;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.measures.CoverageMeasuresBuilder;
import org.sonar.api.measures.Measure;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.persistence.CoverageRecord;
import org.sonar.plugins.ada.persistence.ProjectDAO;

/**
 * Sensor updating coverage metrics.
 *
 * Use the data from GNAThub's database to populate the coverage metrics.
 * This sensor can rely on coverage information provided by Gcov and
 * GNATcoverage.
 */
@Slf4j
@AllArgsConstructor
public class CoverageSensor implements Sensor {
    private final FileSystem fs;
    private final AdaProjectContext adaContext;

    @Override
    public boolean shouldExecuteOnProject(final Project project) {
        return fs.hasFiles(fs.predicates().and(
                fs.predicates().hasType(InputFile.Type.MAIN),
                fs.predicates().hasLanguage(Ada.KEY)));
    }

    @Override
    public void analyse(Project project, SensorContext context) {
        log.info("Computing code coverage");

        if (!adaContext.isDAOLoaded()) {
            log.error("GNAThub db not loaded, cannot load code coverage");
            return;
        }

        File currentFile = null;
        CoverageMeasuresBuilder builder = CoverageMeasuresBuilder.create();
        final ProjectDAO dao = adaContext.getDao();
        final String[] tools = new String[]{"GCov", "GNATcoverage"};

        // Retrieve all coverage data ordered by resource
        for (final String tool : tools) {
            for (final CoverageRecord data : dao.getCoverageByTool(tool)) {
                if (currentFile == null) {
                    currentFile = data.getFile();
                }

                // When switching file:
                //   - save measures for current file,
                //   - create a new builder for measure,
                //   - set current file to the next file
                if (!currentFile.equals(data.getFile())) {
                    saveCoverageMeasureForFile(builder, context, currentFile);
                    builder = CoverageMeasuresBuilder.create();
                    currentFile = data.getFile();
                }

                // Add hits for line
                builder.setHits(data.getLine(), data.getHits());
            }
        }

        if (currentFile != null) {
            // For the last resource
            saveCoverageMeasureForFile(builder, context, currentFile);
        }
    }

    /**
     * Saves the coverage measures associated with a given file.
     *
     * @param builder The measure builder.
     * @param context The context to save each measure.
     * @param resource The resource file to save measure for.
     */
    private void saveCoverageMeasureForFile(
            final CoverageMeasuresBuilder builder,
            final SensorContext context,
            final File resource)
    {
        for (final Measure coverage : builder.createMeasures()) {
            context.saveMeasure(resource, coverage);
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
