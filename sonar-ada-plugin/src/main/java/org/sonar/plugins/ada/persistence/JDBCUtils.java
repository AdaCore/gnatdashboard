/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.persistence;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import org.sonar.plugins.ada.utils.AdaUtils;
import org.sqlite.JDBC;

// /!\ Should be replaced by the use of MyBatis, after solving the classloader
// problem when trying to connext to the DB with MyBatis /!\
public class JDBCUtils {

    public static final String DEFAULT_DB_PATH = "obj" + System.getProperty("file.separator")
                                                       + "gnathub.db";
    public static final String QMT_DB_PATH = "sonar.ada.gnathub.db";
    private static String DBurl;
    private static Connection connection = null;

    private JDBCUtils() {
    }

    public static void setDBurl(String DBPath) {
        DBurl = JDBC.PREFIX + DBPath;
    }

    private static Connection getConnection() {
        if (connection == null) {
            try {
                Class.forName(JDBC.class.getName());
                connection = DriverManager.getConnection(DBurl);
            } catch (SQLException ex) {
                AdaUtils.LOG.error("Could not connect to Qualimetrics database: {}", DBurl);
            } catch (ClassNotFoundException ex) {
                AdaUtils.LOG.error("Driver class nor found: {}", JDBC.class.getName());
            }
        }
        return connection;
    }

    public static <T> List<T> query(final String queryString, RowMapper<T> rowMapper) {
        List<T> result = new ArrayList<T>();
        try {
            PreparedStatement statement = getConnection().prepareStatement(queryString);
            try {
                ResultSet resultSet = statement.executeQuery();
                while (resultSet.next()) {
                    result.add(rowMapper.mapRow(resultSet));
                }
            } finally {
                statement.close();
            }
        } catch (SQLException e) {
            AdaUtils.LOG.error("Query SQL error: " + e.getMessage());
            AdaUtils.LOG.info(e.getLocalizedMessage());
            closeConnection();
        }

        return result;
    }

    public static void closeConnection(){
        try {
            connection.close();
        } catch (SQLException ex) {
            AdaUtils.LOG.error("Cannot close connection to database.");
            AdaUtils.LOG.error(ex.getMessage());
        }
    }
}
