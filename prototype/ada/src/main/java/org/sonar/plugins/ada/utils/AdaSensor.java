/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
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
import org.sonar.plugins.ada.AdaFile;

public abstract class AdaSensor implements Sensor {

    private RuleFinder ruleFinder;
    private Configuration config = null;

    public AdaSensor() {
    }

    public AdaSensor(Configuration config) {
        this.config = config;
    }

    public AdaSensor(RuleFinder ruleFinder, Configuration config) {
        this.ruleFinder = ruleFinder;
        this.config = config;
    }

    public boolean shouldExecuteOnProject(Project project) {
        return Ada.KEY.equals(project.getLanguageKey());
    }

    public void analyse(Project project, SensorContext context) {
        try {
            List<File> reports = getReports(config, project.getFileSystem().getBasedir().getPath(),
                    reportPathKey(), defaultReportPath());
            for (File report : reports) {
                AdaUtils.LOG.info("Processing report '{}'", report);
                processReport(project, context, report);
            }

            if (reports.isEmpty()) {
                handleNoReportsCase(context);
            }
        } catch (Exception e) {
            String msg = new StringBuilder().append("Cannot feed the data into sonar, details: '").append(e).append("'").toString();
            throw new SonarException(msg, e);
        }
    }

    protected List<File> getReports(Configuration conf,
            String baseDirPath,
            String reportPathPropertyKey,
            String defaultReportPath) {
        String reportPath = conf.getString(reportPathPropertyKey, null);
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

    protected void saveViolation(Project project, SensorContext context, String ruleRepoKey,
            String file, int line, String ruleId, String msg, String prj, String dir) {
        RuleQuery ruleQuery = RuleQuery.create().withRepositoryKey(ruleRepoKey).withKey(ruleId);
        Rule rule = ruleFinder.find(ruleQuery);
        if (rule != null) {
            AdaFile resource = AdaFile.fromIOFile(new File(file), project);
            AdaUtils.LOG.info("***** File " + resource.getLongName());
            AdaUtils.LOG.info("\tLong name : " + resource.getLongName());
            AdaUtils.LOG.info("\tName : " + resource.getName());
            AdaUtils.LOG.info("\tParent : " + resource.getParent().getName());
            AdaUtils.LOG.info("\tLanguage : " + resource.getParent().getLanguage().getName());
            if (context.getResource(resource) != null) {
                Violation violation = Violation.create(rule, resource).setLineId(line).setMessage(msg);
                AdaUtils.LOG.info("Saving violation : " + violation.getMessage());
                context.saveViolation(violation);
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

    protected void processReport(Project project, SensorContext context, File report)
            throws Exception {
    }

    protected void handleNoReportsCase(SensorContext context) {
    }

    protected String reportPathKey() {
        return "";
    }

    protected String defaultReportPath() {
        return "";
    }
}
