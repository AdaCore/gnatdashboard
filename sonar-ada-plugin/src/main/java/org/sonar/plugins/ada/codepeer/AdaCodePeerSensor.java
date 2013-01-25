/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.RuleFinder;
import org.sonar.plugins.ada.utils.AdaSensor;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Sensor for Codepeer external tool, retrieve informations from a report and
 * save the measures.
 */
public class AdaCodePeerSensor extends AdaSensor {

    /**
     * Sonar property key for Codepeer report absolute path
     */
    public static final String REPORT_PATH_KEY = "sonar.ada.codepeer.reportPath";

    public AdaCodePeerSensor(RuleFinder ruleFinder, Configuration conf) {
        super(ruleFinder, conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }

    @Override
    protected String defaultReportPath() {
        return "reports/codepeer-report.json";
    }

    /**
     * Parse Codepeer JSON report to retrieve violations information.
     *
     * @param project current analyzed project
     * @param context Sensor's context
     * @param report Codepeer JSON report
     */
    @Override
    protected void processReport(final SensorContext context, File report) {
        AdaUtils.LOG.info("--------- Codepeer sensor");

        try {
            JSONParser parser = new JSONParser();
            JSONObject jsonTree = (JSONObject) parser.parse(new FileReader(report));

            JSONArray errors = (JSONArray) jsonTree.get("errors");

            for (Object o : errors) {
                JSONObject error = (JSONObject) o;
                String file = (String) error.get("src");
                String ruleKey = (String) error.get("rule_key");
                String dir = (String) error.get("directory");
                String line = (String) error.get("line");
                String msg = (String) error.get("msg");
                String prj = (String) error.get("prj");

                if (isInputValid(file, ruleKey, dir, line, msg, prj)) {

                   saveViolation(context, AdaCodePeerRuleRepository.KEY,
                            file, Integer.parseInt(line), ruleKey, msg, prj, dir);
                } else {
                    AdaUtils.LOG.warn("Skipping violation, information are not complete for message: {}", msg);
                }
            }
        } catch (IOException ex) {
            AdaUtils.LOG.warn("Unable to process report : {}, error: { }", report.getName(), ex.getMessage());
        } catch (ParseException ex) {
            AdaUtils.LOG.warn("Unable to parse report: {}, error {}", report.getName(), ex.getMessage());
        }
    }

    /**
     * Check that retrieved information from Codepeer report are valid: every
     * parameter must not be null or empty
     *
     * @param file
     * @param ruleKey
     * @param directory
     * @param line
     */
    private boolean isInputValid(String file, String ruleKey, String dir, String line,
            String msg, String prj) {
        return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(ruleKey)
                && !StringUtils.isEmpty(dir) && !StringUtils.isEmpty(line)
                && !StringUtils.isEmpty(msg) && !StringUtils.isEmpty(prj);
    }


}
