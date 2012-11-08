/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import java.io.File;
import javax.xml.stream.XMLStreamException;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.codehaus.staxmate.in.SMHierarchicCursor;
import org.codehaus.staxmate.in.SMInputCursor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.utils.SonarException;
import org.sonar.api.utils.StaxParser;
import org.sonar.plugins.ada.AdaMetrics;
import org.sonar.plugins.ada.AdaSourceImporter;
import org.sonar.plugins.ada.resources.AdaFile;
import org.sonar.plugins.ada.utils.AdaSensor;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Sensor for GNAT Metric external tool, retrieve informations from a report and
 * save the measures.
 */
public class AdaGnatMetricSensor extends AdaSensor {

    /**
     * Sonar property key for GNAT Metric report absolute path
     */
    public static final String REPORT_PATH_KEY = "sonar.ada.gnatmetric.reportPath";

    public AdaGnatMetricSensor(Configuration conf) {
        super(conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }

    /**
     * Parse GNAT Metric XML report to retrieve violations information.
     *
     * @param project current analyzed project
     * @param context Sensor's context
     * @param report GNAT Metric XML report
     */
    @Override
    protected void processReport(final Project project, final SensorContext context, File report)
            throws javax.xml.stream.XMLStreamException {
        AdaUtils.LOG.info("---- GnatMetric Analyser");
        StaxParser parser = new StaxParser(new StaxParser.XmlStreamHandler() {

            /**
             * {@inheritDoc}
             */
            public void stream(SMHierarchicCursor rootCursor) throws XMLStreamException {
                rootCursor.advance(); //global

                SMInputCursor fileCursor = rootCursor.childElementCursor("file"); //file
                while (fileCursor.getNext() != null) {
                    String file = fileCursor.getAttrValue("name");

                    SMInputCursor metricCursor = fileCursor.childElementCursor("metric"); //metric
                    while (metricCursor.getNext() != null) {
                        String metrickey = metricCursor.getAttrValue("name");
                        Double value = metricCursor.getElemDoubleValue();
                        //Save measure if retrieved values are valid
                        if (isInputValid(file, metrickey, value)) {
                            saveMeasure(context, file, metrickey, value);
                        } else {
                            AdaUtils.LOG.warn("AdaGnatMetric warning, skipping metric: {}", metrickey);
                        }
                    }
                }
            }

            /**
             * Check that retrieved information from GNAT Metric report are
             * valid: every parameter must not be null or empty
             *
             * @param file
             * @param ruleKey
             * @param directory
             * @param line
             */
            public boolean isInputValid(String file, String name, Double value) {
                return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(name) && value != null;
            }
        });

        parser.parse(report);
    }

    /**
     * Save a measure if the corresponding file and metric are found.
     *
     * File existence is check via AdaSourceImporter#sourceMap Metric existence
     * is check via AdaMetrics#metricByKey
     *
     * @param context
     * @param file
     * @param metrickey
     * @param value
     */
    public void saveMeasure(SensorContext context, String fileName, String metrickey, Double value) {
        String file = StringUtils.substringAfterLast(fileName, "/");
        AdaFile resource = AdaSourceImporter.getSourceMap().get(file);
        if (resource != null) {
            Metric metric = AdaMetrics.INSTANCE.getMetricsMap().get(metrickey);
            if (metric != null) {
                try {
                    context.saveMeasure(resource, metric, value);
                } catch (SonarException e) {
                    AdaUtils.LOG.info(e.getMessage());
                }
            } else {
                AdaUtils.LOG.info("Skipping metric because unknown metric key: {}", metrickey);
            }
        } else {
            AdaUtils.LOG.warn("Connot find resource {}, skipping metric: {}", file, metrickey);
        }
    }
}
