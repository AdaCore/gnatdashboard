/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gcov;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.PersistenceMode;
import org.sonar.api.measures.PropertiesBuilder;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.utils.ParsingUtils;
import org.sonar.plugins.ada.api.resource.AdaFile;
import org.sonar.plugins.ada.utils.AdaSensor;
import org.sonar.plugins.ada.utils.AdaUtils;

public class GCovSensor extends AdaSensor {

    public static final String REPORT_PATH_KEY = "sonar.ada.gcov.reportPath";

    public GCovSensor(RuleFinder ruleFinder, Configuration conf) {
        super(ruleFinder, conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }

    /**
     * Parse GCov JSON report to retrieve coverage information.
     *
     * @param project current analyzed project
     * @param context Sensor's context
     * @param report GCov JSON report
     */
    @Override
    protected void processReport(final SensorContext context, File report) {
        AdaUtils.LOG.info("--------- GCov sensor");

        try {
            JSONParser parser = new JSONParser();
            JSONObject jsonTree = (JSONObject) parser.parse(new FileReader(report));

            JSONArray errors = (JSONArray) jsonTree.get("errors");

            for (Object o : errors) {
                JSONObject error = (JSONObject) o;
                String file = (String) error.get("src");
                String dir = (String) error.get("directory");
                String prj = (String) error.get("prj");

                AdaFile res = context.getResource(AdaFile.fromIOFile(new File(file), prj, dir));


                if (isInputValid(file, dir, prj)) {
                    if (res != null) {
                        JSONObject hits = (JSONObject) error.get("hits");
                        saveFileData(res, hits, context);

                    } else {
                        AdaUtils.LOG.info("Skipping coverage measure, cannot find the file '{}' ", file);
                    }

                } else {
                    AdaUtils.LOG.warn("Skipping coverage measure, information not complete for source: {}", file);
                }

            }
        } catch (IOException ex) {
            AdaUtils.LOG.warn("Unable to process report : {}, error: { }", report.getName(), ex.getMessage());
        } catch (ParseException ex) {
            AdaUtils.LOG.warn("Unable to parse report: {}, error {}", report.getName(), ex.getMessage());
        }
    }

    /**
     * Check that retrieved information from GCov report are valid: every
     * parameter must not be null or empty
     *
     * @param file
     * @param directory
     * @param prj
     */
    private boolean isInputValid(String file, String dir, String prj) {
        return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(dir)
                && !StringUtils.isEmpty(prj);
    }

    private void saveFileData(AdaFile resource, JSONObject hits, final SensorContext context) {
        FileData data = new FileData(resource);
        for (Object obj : hits.keySet()) {
            String line = (String) obj;
            String hitsLine = (String) hits.get(obj);
            if (!StringUtils.isEmpty(line) && !StringUtils.isEmpty(hitsLine)) {
                try {
                    data.addLine(line, Long.parseLong(hitsLine));
                } catch (NumberFormatException e) {
                    AdaUtils.LOG.warn("Invalid value for number of hit: " + hitsLine
                            + ", considering the line as uncovered at {}:{}", line, resource.getName());
                    data.addLine(line, 0l);
                }
            }
        }
        for (Measure measure : data.getMeasures()) {
            context.saveMeasure(resource, measure);
        }
    }

    private class FileData {

        private int lines = 0;
        private int conditions = 0;
        private int coveredLines = 0;
        private int coveredConditions = 0;
        private Resource<?> file;
        private PropertiesBuilder<String, Long> lineHitsBuilder = new PropertiesBuilder<String, Long>(CoreMetrics.COVERAGE_LINE_HITS_DATA);
        private PropertiesBuilder<String, String> branchHitsBuilder = new PropertiesBuilder<String, String>(CoreMetrics.BRANCH_COVERAGE_HITS_DATA);

        public void addLine(String lineId, Long lineHits) {
            lines++;
            if (lineHits > 0) {
                coveredLines++;
            }
            lineHitsBuilder.add(lineId, lineHits);
        }

        public void addConditionLine(String lineId, int coveredConditions, int conditions, String label) {
            this.conditions += conditions;
            this.coveredConditions += coveredConditions;
            branchHitsBuilder.add(lineId, label);
        }

        public FileData(Resource<?> file) {
            this.file = file;
        }

        public List<Measure> getMeasures() {
            List<Measure> measures = new ArrayList<Measure>();
            if (lines > 0) {
                measures.add(new Measure(CoreMetrics.COVERAGE, calculateCoverage(coveredLines + coveredConditions, lines + conditions)));

                measures.add(new Measure(CoreMetrics.LINE_COVERAGE, calculateCoverage(coveredLines, lines)));
                measures.add(new Measure(CoreMetrics.LINES_TO_COVER, (double) lines));
                measures.add(new Measure(CoreMetrics.UNCOVERED_LINES, (double) lines - coveredLines));
                measures.add(lineHitsBuilder.build().setPersistenceMode(PersistenceMode.DATABASE));

                if (conditions > 0) {
                    measures.add(new Measure(CoreMetrics.BRANCH_COVERAGE, calculateCoverage(coveredConditions, conditions)));
                    measures.add(new Measure(CoreMetrics.CONDITIONS_TO_COVER, (double) conditions));
                    measures.add(new Measure(CoreMetrics.UNCOVERED_CONDITIONS, (double) conditions - coveredConditions));
                    measures.add(branchHitsBuilder.build().setPersistenceMode(PersistenceMode.DATABASE));
                }
            }
            return measures;
        }

        public Resource<?> getFile() {
            return file;
        }
    }

    private double calculateCoverage(int coveredElements, int elements) {
        if (elements > 0) {
            return ParsingUtils.scaleValue(100.0 * ((double) coveredElements / (double) elements));
        }

        return 0.0;
    }
}
