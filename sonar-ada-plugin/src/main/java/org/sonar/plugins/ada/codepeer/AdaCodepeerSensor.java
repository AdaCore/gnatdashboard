/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
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

public class AdaCodepeerSensor extends AdaSensor {

    public static final String REPORT_PATH_KEY = "sonar.ada.codepeer.reportPath";

    public AdaCodepeerSensor(RuleFinder ruleFinder, Configuration conf) {
        super(ruleFinder, conf);
    }

    @Override
    protected String reportPathKey() {
        return REPORT_PATH_KEY;
    }

    @Override
    protected void processReport(final Project project, final SensorContext context, File report) {
         AdaUtils.LOG.info("--------- Codepeer sensor");
        try {
            JSONParser parser = new JSONParser();
            JSONObject jsonTree = (JSONObject) parser.parse(new FileReader(report));

            JSONArray errors = (JSONArray) jsonTree.get("errors");

            for (Object o : errors) {
                JSONObject error = (JSONObject) o;
                String file = (String) error.get("src");
                String rule_key = (String) error.get("rule_key");
                String dir = (String) error.get("directory");
                String line = (String) error.get("line");
                String msg = (String) error.get("msg");
                String prj = (String) error.get("prj");

                if (isInputValid(file, rule_key, dir, line, msg, prj)) {
                    saveViolation(project, context, AdaCodepeerRuleRepository.KEY,
                            file, Integer.parseInt(line), rule_key, msg, prj, dir);
                } else {
                    AdaUtils.LOG.warn("AdaCheck warning: {}", msg);
                }
            }

        } catch (IOException ex) {
            AdaUtils.LOG.warn("Unable to process report : {}, error: {}", report.getName(), ex.getMessage());
        } catch (ParseException ex) {
            AdaUtils.LOG.warn("Unable to parse report: {}, error {}", report.getName(), ex.getMessage());
        }
    }

    private boolean isInputValid(String file, String rule_key, String dir, String line,
            String msg, String prj) {
        return !StringUtils.isEmpty(file) && !StringUtils.isEmpty(rule_key)
                && !StringUtils.isEmpty(dir) && !StringUtils.isEmpty(line)
                && !StringUtils.isEmpty(msg) && !StringUtils.isEmpty(prj);
    }
}
