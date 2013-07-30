/*
 * Sonar Requirements Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.requirements;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import org.apache.commons.configuration.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sqlite.JDBC;

/**
 * Feeds sonar with project requirements data
 */
public class RequirementsSensor implements Sensor {

    // <editor-fold desc="Attributes" defaultstate="Collapsed">
    private Configuration conf;
    private final Logger log = LoggerFactory.getLogger(RequirementsSensor.class);

    // Query to retrieve all measure related to requirements
    private final String MEASURE_QUERY =
            "SELECT m.data as value, r.identifier as metric_key, rm.resource_id "
            + "FROM resources_messages rm, rules r, tools t, messages m "
            + "WHERE m.rule_id = r.id "
            + "AND rm.message_id = m.id "
            + "AND r.tool_id = t.id "
            + "AND m.rule_id = r.id "
            + "AND r.kind = 1 "
            + "AND UPPER(t.name) = UPPER('requirements');";

    // Query to retrieve resource information from its id.
    private final String RESOURCE_QUERY =
            "SELECT child.name as child, parent.name as parent "
            + "FROM resources child, resources parent, resource_trees tree "
            + "WHERE child.id = tree.child_id "
            + "AND parent.id = tree.parent_id "
            + "AND child.id = ?";
    // </editor-fold>

    public RequirementsSensor(Configuration conf) {
        this.conf = conf;
    }

    /**
     * Build SQLite URL string for the given file path
     */
    private String getDBurl(String DBPath) {
        log.debug("Sqlite DB path {}", DBPath);
        return JDBC.PREFIX + DBPath;
    }

    @Override
    public void analyse(Project project, SensorContext context) {
        log.info("---- Starting Requirements sensor");

        // Initialise DB object
        Connection connection = null;
        PreparedStatement measureStm = null;
        PreparedStatement resourceStm = null;

        try {
            // Retrieve Directory class from Ada Sonar Plugin
            // used to retrieve resource reference from Sonar
            Class adaDirectory = getClass().getClassLoader()
                    .loadClass("org.sonar.plugins.ada.api.resource.AdaDirectory");

            // Retrieve AdaDirectory constructor
            Class[] types = new Class[]{String.class, String.class};
            Constructor construct = adaDirectory.getConstructor(types);

            // Create dataBase Objects
            Class.forName(JDBC.class.getName());
            connection = DriverManager.getConnection(getDBurl(conf.getString("sonar.ada.qmt.db.path")));
            measureStm = connection.prepareStatement(MEASURE_QUERY);
            resourceStm = connection.prepareStatement(RESOURCE_QUERY);

            // Fetch measures from DB
            ResultSet measureSet = measureStm.executeQuery();
            while (measureSet.next()) {

                // Create sonar measure
                Measure measure = new Measure(measureSet.getString("metric_key"));
                measure.setValue(measureSet.getDouble("value"));
                log.debug("Measur for: {} - value: {}", measure.getMetricKey(), measure.getValue());

                // Fetch associated directory from DB
                resourceStm.setInt(1, measureSet.getInt("resource_id"));
                ResultSet resourceSet = resourceStm.executeQuery();

                // Instanciate AdaDirectory through reflection
                Object[] constructPram = new String[]{resourceSet.getString("child"),
                                                      resourceSet.getString("parent")};
                Object directory = construct.newInstance(constructPram);

                // Retrieve corresponding resource reference from sonar
                Resource resource = context.getResource((Resource) directory);
                log.debug("** Resource: {}", resource);

                if (resource != null){
                    context.saveMeasure(resource, measure);
                } else {
                    log.warn("Skipping measure: {}, becasue associated resource not found: {}",
                            measure.getMetricKey(), resource.getName());
                }

            }

        } catch (InstantiationException ex) {
            log.error("InstantiationException: ", ex);
        } catch (IllegalAccessException ex) {
            log.error("IllegalAccessException: ", ex);
        } catch (IllegalArgumentException ex) {
            log.error("IllegalArgumentException: ", ex);
        } catch (InvocationTargetException ex) {
            log.error("InvocationTargetException: ", ex);
        } catch (NoSuchMethodException ex) {
            log.error("NoSuchMethodException: ", ex);
        } catch (SecurityException ex) {
            log.error("SecurityException: ", ex);
        } catch (SQLException ex) {
            log.error("SQLException: ", ex);
        } catch (ClassNotFoundException ex) {
            log.error("ClassNotFoundException: ", ex);
        } finally {
            try {
                measureStm.close();
                resourceStm.close();
                connection.close();
            } catch (SQLException ex) {
                 log.error("SQLException when trying to clean Java SQL : ", ex);
            }
        }
    }

    @Override
    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguageKey().equalsIgnoreCase("ada");
    }
}
