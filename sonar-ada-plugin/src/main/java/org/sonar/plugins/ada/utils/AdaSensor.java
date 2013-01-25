/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.configuration.Configuration;
import org.apache.tools.ant.DirectoryScanner;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.RuleQuery;
import org.sonar.api.rules.Violation;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.resources.AdaFile;

/**
 * Generic Sensor for external tool, retrieve informations from reports.
 */
public abstract class AdaSensor implements Sensor {

    private RuleFinder ruleFinder;
    private Configuration config = null;


    public AdaSensor(Configuration config) {
        this.config = config;
    }

    public AdaSensor(RuleFinder ruleFinder, Configuration config) {
        this.ruleFinder = ruleFinder;
        this.config = config;
    }

    /**
     * This sensor is executed only for Ada projects.
     */
    public boolean shouldExecuteOnProject(Project project) {
        return Ada.KEY.equals(project.getLanguageKey());
    }

    public void analyse(Project project, SensorContext context) {
        try {
            List<File> reports = getReports(config, project.getFileSystem().getBasedir().getPath(),
                    reportPathKey(), defaultReportPath());
            for (File report : reports) {
                AdaUtils.LOG.info("Processing report '{}'", report);
                processReport(context, report);
            }

            if (reports.isEmpty()) {
                handleNoReportsCase(context);
            }
        } catch (Exception e) {
            String msg = new StringBuilder().append("Cannot feed the data into sonar, details: '").append(e).append("'").toString();
            throw new SonarException(msg, e);
        }
    }

    /**
     * Retrieve report on the file system.
     * @return List of reports
     */
    protected List<File> getReports(Configuration conf,
            String baseDirPath,
            String reportPathPropertyKey,
            String defaultReportPath) {
        String reportPath = conf.getString(reportPathPropertyKey);
        System.out.println(reportPath);
        if (reportPath == null) {
            reportPath = defaultReportPath;
        }

        AdaUtils.LOG.info("Using pattern '{}' to find reports", reportPath);

        DirectoryScanner scanner = new DirectoryScanner();
        String[] includes = new String[1];
        includes[0] = reportPath;
        scanner.setIncludes(includes);
        scanner.setBasedir(new File(baseDirPath));
        scanner.scan();
        String[] relPaths = scanner.getIncludedFiles();

        List<File> reports = new ArrayList<File>();
        for (String relPath : relPaths) {
            reports.add(new File(baseDirPath, relPath));
        }

        return reports;
    }

    /**
     * Save a violation if the rule and the file are found.
     *
     * @return Boolean: used by Codepeer Sensor to save category/severity
     *                  violation metric measure as we don't want to save this
     *                  measure if the violation has not been save. It will may
     *                  be necessary to manage this differently in the future
     *                  (through decorator)
     */
    protected void saveViolation(SensorContext context, String ruleRepoKey,
        String file, int line, String ruleId, String msg, String prj, String dir) {
        RuleQuery ruleQuery = RuleQuery.create().withRepositoryKey(ruleRepoKey).withKey(ruleId);
        Rule rule = ruleFinder.find(ruleQuery);
        if (rule != null) {
            AdaFile res = context.getResource(AdaFile.fromIOFile(new File(file), prj, dir));

            if (res != null) {
                Violation violation = Violation.create(rule, res).setLineId(line).setMessage(msg);
                context.saveViolation(violation);
                /**
                 * As it is not possible to check right after the call of
                 * context#saveViolation() if the violation has really been saved or not:
                 *  - reproduce one condition of DefaultIndex#addViolation()
                 *  - doesn't manage the case of ViolationFilters (see DefaultIndex#addViolation())
                 */

            } else {
                AdaUtils.LOG.info("Cannot find the file '{}', skipping violation '{}'", file, msg);
            }
        } else {
            AdaUtils.LOG.warn("Cannot find the rule {}, skipping violation", ruleId);
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }

    /**
     * Retrieve information from the report.
     */
    protected abstract void processReport(final SensorContext context, File report);
    protected void handleNoReportsCase(SensorContext context){
    }
    /**
     * @return property's key of the report path in sonar.properties file
     */
    protected String reportPathKey() {
        return "";
    }

    protected String defaultReportPath() {
        return "";
    }
}
