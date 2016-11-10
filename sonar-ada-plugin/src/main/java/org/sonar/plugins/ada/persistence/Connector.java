/*
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2016, AdaCore
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package org.sonar.plugins.ada.persistence;

import lombok.AllArgsConstructor;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;
import org.sqlite.JDBC;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Connector object to access the project database.
 *
 * Note: Should be replaced by the use of MyBatis, after solving the class
 * loader problem when trying to connect to the DB with MyBatis.
 */
@Slf4j
@AllArgsConstructor
public class Connector {
  private final String dbPath;

  /**
   * @return The full URL to connect to the DB.
   */
  public String dbUrl() {
    return JDBC.PREFIX + dbPath;
  }

  /**
   * Executes a query on the database, map the results and returns them.
   *
   * @param sql    The SQL query to perform.
   * @param mapper The row mapper to use to map the result of the query.
   * @return The list of results.
   */
  public <T> List<T> query(final String sql, RowMapper<T> mapper) {
    log.debug("Executing SQL query: {}", sql);

    final List<T> rows = new ArrayList<T>();

    try {
      @Cleanup final Connection conn = createConnection();
      @Cleanup final PreparedStatement ps = conn.prepareStatement(sql);
      final ResultSet resultSet = ps.executeQuery();

      while (resultSet.next()) {
        final T row = mapper.mapRow(resultSet);
        if (row != null) {
          rows.add(row);
        }
      }
    } catch (SQLException why) {
      log.error("SQL query error: {}", why.getMessage(), why);
    }

    return rows;
  }

  /**
   * Opens a new connection to the database.
   * <p>
   * It is the user responsibility to close the connection to database using the
   * {@code #closeConnection()} method.
   *
   * @return The new connection object used to interact with the database.
   */
  private Connection createConnection() {
    try {
      Class.forName(JDBC.class.getName());

      log.debug("Opening new connection to {}", dbUrl());
      return DriverManager.getConnection(dbUrl());

    } catch (SQLException why) {
      log.error("GNAThub database connection failed: {}", dbUrl(), why);
    } catch (ClassNotFoundException why) {
      log.error("JDBC driver not found: {}", JDBC.class.getName(), why);
    }

    return null;
  }
}
