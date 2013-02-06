/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
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
import org.sonar.api.utils.SonarException;
import org.sonar.api.utils.StaxParser;
import org.sonar.plugins.ada.AdaMetrics;
import org.sonar.plugins.ada.AdaSourceImporter;
import org.sonar.plugins.ada.resources.AdaFile;
import org.sonar.plugins.ada.utils.AdaSensor;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Sensor for GNATmetric external tool, retrieve informations from a report and
 * save the measures.
 */
public class AdaGnatMetricSensor extends AdaSensor {

    /**
     * Sonar property key for GNATmetric report absolute path
     */
    public static final String REPORT_PATH_KEY = "sonar.ada.gnatmetric.reportPath";

    public AdaGnatMetricSensor(Configuration conf) {
        super(conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }


    @Override
    protected String defaultReportPath() {
        return "reports/gnatmetric-report.xml";
    }

    /**
     * Parse GNATmetric XML report to retrieve violations information.

     * @param project current analyzed project
     * @param context Sensor's context
     * @param report GNATmetric XML report
     */
    @Override
    protected void processReport(final SensorContext context, File report){
        AdaUtils.LOG.info("---- GnatMetric Analyser");
        StaxParser parser = new StaxParser(new StaxParser.XmlStreamHandler() {

            /**
             * {@inheritDoc}
             */
            public void stream(SMHierarchicCursor rootCursor) throws XMLStreamException {
                /** In GNAT metric output, complexity is reported for
                 * specification files only if it contains a function
                 * expression, otherwise complexity is not reported at all
                 * However, 0 is considered as the implicit value of
                 * complexity; as the cyclomatic complexity is mapped on the
                 * Sonar complexity specific process has to be done for this
                 * metric in order to set the value to 0 when it is not mentioned
                **/
                boolean forceZeroForSonarComplexity = true;

                //global
                rootCursor.advance();

                // <file>
                SMInputCursor fileCursor = rootCursor.childElementCursor("file");
                while (fileCursor.getNext() != null) {
                    // Re-initialized for each file
                    forceZeroForSonarComplexity = true;
                    String file = fileCursor.getAttrValue("name");

                    // <metric>
                    SMInputCursor metricCursor = fileCursor.childElementCursor("metric");
                    while (metricCursor.getNext() != null) {
                        String metrickey = metricCursor.getAttrValue("name");
                        Double value = metricCursor.getElemDoubleValue();

                        //Save measure if retrieved values are valid
                        if (isInputValid(file, metrickey, value)) {

                            saveMeasure(context, file, metrickey, value);

                            if (metrickey.equals(AdaMetrics.CYCLOMATIC_COMPLEXITY)){
                                forceZeroForSonarComplexity = false;
                            }
                        } else {
                            AdaUtils.LOG.warn("AdaGnatMetric warning:" +
                           "metric's inputs are invalid, skipping metric: {}",
                           metrickey);
                        }
                    }
                    if (forceZeroForSonarComplexity){
                        saveMeasure(context, file, AdaMetrics.CYCLOMATIC_COMPLEXITY, 0d);
                    }
                }
            }

            /**
             * Check that retrieved information from GNATmetric report are
             * valid: every parameter must not be null or empty

             * @param file
             * @param ruleKey
             * @param directory
             * @param line
             */
            public boolean isInputValid(String file, String name, Double value) {
                return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(name) && value != null;
            }
        });
        try {
            parser.parse(report);
        } catch (XMLStreamException ex) {
            AdaUtils.LOG.error("Unable to parse report: {}", report.getName());
            AdaUtils.LOG.error(ex.getMessage());
        }
    }

    /**
     * Save a measure if the corresponding file and metric are found.

     * File existence is check via AdaSourceImporter#sourceMap Metric existence
     * is check via AdaMetrics#metricByKey

     * @param context
     * @param file
     * @param metrickey
     * @param value
     */
    private void saveMeasure(SensorContext context, String fileName, String metrickey, Double value) {
        String file = StringUtils.substringAfterLast(fileName, "/");
        // /!\ To be replace by context.getResource(new File(..)) and remove srcMap.
        // Add full path of source in GNAT metric report
        AdaFile resource = AdaSourceImporter.getSourceMap().get(file);
        // Check that file exist in imported tree
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
