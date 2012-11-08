/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.gnatmetric.GnatMetrics;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Defines metrics used by the Ada Sonar Plugin
 */
public final class AdaMetrics {

    public static final AdaMetrics INSTANCE = new AdaMetrics();
    /**
     * GnatMetric keys
     */
    public static final String ALL_LINES = "all_lines";
    public static final String CODE_LINES = "code_lines";
    public static final String BLANK_LINES = "blank_lines";

    public static final String COMMENT_LINES = "comment_lines";
    public static final String EOL_COMMENTS = "eol_comments";
    public static final String COMMENT_PERCENTAGE = "comment_percentage";

    public static final String ALL_SATEMENTS = "all_stmts";
    public static final String ALL_DCLS = "all_dcls";
    public static final String LSLOC = "lsloc";

    public static final String CONSTRUCT_NESTING = "construct_nesting";
    public static final String MAX_LOOP_NESTING = "max_loop_nesting";
    public static final String EXTRA_EXIT_POINTS = "extra_exit_points";

    public static final String STATEMENT_COMPLEXITY = "statement_complexity";
    public static final String SHORT_CIRCUIT_COMPLEXITY = "short_circuit_complexity";
    public static final String CYCLOMATIC_COMPLEXITY = "cyclomatic_complexity";
    public static final String ESSENTIAL_COMPLEXITY = "essential_complexity";

    /**
     * Mapping between GnatMetric keys and Sonar metrics keys.
     * This mapping is necessary since metrics from GNAT Tools are binded on
     * Sonar metrics
     */
    private Map<String, Metric> metricByKey;

    /**
     * Property file's name of mapping between GnatMetric's key and Sonar
     * metric's key.
     * The configuration file has been put aside for the moment since it is
     * complicated to synchronize this information with the GWT module which
     * is in charge of displaying metrics from GnatMetric
     */
    private static final String METRICS_PROPERTIES_PATH = "conf/metrics.properties".replace("/", System.getProperty("file.separator"));

    private AdaMetrics() {
        metricByKey = new HashMap<String, Metric>();
        //As the configuration file is not suported for now.
        //loadMetrics(project);
    }

    /**
     * If the map is empty, fill it with the corresponding metric in Sonar and
     * the one defined in GnatMetric.
     *
     * @return map<String key, Metric metric>
     */
    public Map<String, Metric> getMetricsMap() {
        if (metricByKey.isEmpty()) {

            metricByKey.put(ALL_LINES, CoreMetrics.LINES);
            metricByKey.put(CODE_LINES, CoreMetrics.NCLOC);
            metricByKey.put(COMMENT_LINES, CoreMetrics.COMMENT_LINES);
            metricByKey.put(COMMENT_PERCENTAGE, CoreMetrics.COMMENT_LINES_DENSITY);
            metricByKey.put(CYCLOMATIC_COMPLEXITY, CoreMetrics.COMPLEXITY);
            for (Metric m : GnatMetrics.INSTANCE.getMetrics()) {
                metricByKey.put(m.getKey(), m);
            }
        }
        return metricByKey;
    }

    /**
     * Retrieve metric's key mapping between GnatMetric and Sonar metrics from
     * a property file; creates the file if it does not exist.
     * Is not use for now, see METRICS_PROPERTIES_PATH attribute comment.
     */
    private void loadMetrics(Project project) {
        Properties prop = new Properties();
        File file = new File(project.getFileSystem().getBasedir(), METRICS_PROPERTIES_PATH);
        if (!file.exists()) {
            dumpMetrics(file);
            AdaUtils.LOG.info("Dumps GnatMetric's properties file");
        } else {
            try {
                prop.load(new FileInputStream(file));

                for (Object key : prop.keySet()) {
                    //Call the map via the getter to initialize it.
                    Metric value = getMetricsMap().get(key.toString());
                    //If the metric key already exist but with a different value or does not exist
                    if ((value != null && !value.getKey().equals(prop.getProperty(key.toString())))
                            || value == null) {
                        //Retrieve the Sonar metric, if the key have already existed in
                        //the map, only the metric is replaced.
                        AdaUtils.LOG.info("Set metric's map from properties");
                        Metric sonarmetric = getSonarMetricByKey(prop.getProperty(key.toString()));
                        if (sonarmetric != null) {

                            metricByKey.put(key.toString(), sonarmetric);
                        } else {
                            AdaUtils.LOG.info("Skipping metric, unable to found associated sonar metric with metric key: {}", prop.getProperty(key.toString()));
                        }
                    }
                }
            } catch (IOException ex) {
                AdaUtils.LOG.warn("Unable to load metrics form the property file");
            }
        }
    }

    /**
     * Retrieve a Sonar metric from its key
     *
     * @param key metric key
     * @return metric or null if no metric found
     */
    public Metric getSonarMetricByKey(String key) {
        for (Metric m : CoreMetrics.getMetrics()) {
            if (m.getKey().equals(key)) {
                return m;
            }
        }
        return null;
    }

    /**
     * Create a the mapping property file
     */
    private void dumpMetrics(File file) {
        try {
            Properties prop = new Properties();

            for (String key : getMetricsMap().keySet()) {

                //Set propety's key to GnatMetric key and property's value
                //to the corresponding sonar metric's key (or GnatMetrics#Metric)
                prop.setProperty(key, metricByKey.get(key).getKey());
            }
            file.createNewFile();
            prop.store(new FileOutputStream(file), "Mapping between GnatMetric's key and correpondant Sonar metric's key");
        } catch (IOException ex) {
            AdaUtils.LOG.info("Unable to dump metric's configuration to a property file");
            AdaUtils.LOG.info(ex.getMessage());
        }
    }
}
