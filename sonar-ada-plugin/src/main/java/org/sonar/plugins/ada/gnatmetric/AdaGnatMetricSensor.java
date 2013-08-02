/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import org.apache.commons.configuration.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.api.resource.AdaFile;
import org.sonar.plugins.ada.persistence.AdaDao;
import org.sonar.plugins.ada.utils.Pair;

/**
 * Sensor for GNATmetric external tool, retrieve informations from a report and
 * save the measures.
 */
public class AdaGnatMetricSensor implements Sensor {

    private AdaDao dao = new AdaDao();
    private final Logger log = LoggerFactory.getLogger(AdaGnatMetricSensor.class);

    public AdaGnatMetricSensor(Configuration conf) {
    }

    @Override
    public void analyse(Project project, SensorContext context) {
        for (Pair<AdaFile, Measure> fileMeasure : dao.selectMeasureForTool("GNATmetric")) {
            try {
               Metric metric = GnatMetrics.Metrics.valueOf(fileMeasure.getRight().getMetricKey().toUpperCase()).getMetric();
               Measure measure = new Measure(metric, fileMeasure.getRight().getValue());
               AdaFile res = context.getResource(fileMeasure.getLeft());

               if (res != null){
                   context.saveMeasure(res, measure);

               } else {
                   log.warn("Cannot find the file '{}', skipping metric '{}'", fileMeasure.getLeft().getName());
               }
            } catch (IllegalArgumentException e) {
                log.warn("Skipping measure, metric not found: {}", fileMeasure.getRight().getMetricKey());
            }

        }
    }

    @Override
    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguageKey().equals(Ada.KEY);
    }
}
