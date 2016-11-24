/*
 * GNATdashboard
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

package com.adacore.gnatdashboard.gnathub.api.orm;

import com.google.common.annotations.VisibleForTesting;
import lombok.extern.slf4j.Slf4j;
import org.sqlite.JDBC;

import java.io.File;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Connector object to access the project database.
 *
 * Note: Should be replaced by the use of MyBatis, after solving the class
 * loader problem when trying to connect to the DB with MyBatis.
 */
@Slf4j
public class Connector {
  private final String dbPath;
  private Connection connection;

  @VisibleForTesting
  public Connector(final File db) {
    this(db.getAbsolutePath());
  }

  public Connector(final String dbPath) {
    loadJDBCDriver();
    this.dbPath = dbPath;
  }

  /**
   * @return The full URL to connect to the DB.
   */
  private String dbUrl() {
    return JDBC.PREFIX + dbPath;
  }

  /**
   * Opens a new connection to the database.
   *
   * It is the user responsibility to close the connection to database using the
   * {@link #closeConnection()} method.
   *
   * @return The new connection object used to interact with the database.
   * @throws SQLException
   */
  public void openConnection() throws SQLException {
    log.debug("Opening connection to {}", dbUrl());
    connection = DriverManager.getConnection(dbUrl());
  }

  @SuppressWarnings("unused")
  public void closeConnection() throws SQLException {
    if (connection != null) {
      log.debug("Closing connection to {}", dbUrl());
      connection.close();
      connection = null;
    }
  }

  /**
   * Creates a prepared statement.
   *
   * @param sql The SQL query to initialize the prepared statement.
   * @return The prepared statement.
   * @throws SQLException
   */
  PreparedStatement createStatement(final String sql) throws SQLException {
    Objects.requireNonNull(connection);
    return connection.prepareStatement(sql);
  }


  /**
   * Executes a query on the database, map the results and returns them.
   *
   * {@link #openConnection()} must be called prior to any call to this method.
   * The user is responsible for closing the {@link PreparedStatement}.
   *
   * @param stm The {@link PreparedStatement} to exectute.
   * @param mapper The row mapper to use to map the result of the query.
   * @return The list of results.
   * @throws SQLException
   */
  public <T> List<T> query(final PreparedStatement stm, RowMapper<T> mapper) throws SQLException {
    final List<T> rows = new ArrayList<>();
    final ResultSet resultSet = stm.executeQuery();

    while (resultSet.next()) {
      Optional.ofNullable(mapper.mapRow(resultSet)).ifPresent(rows::add);
    }
    return rows;
  }

  /**
   * Ensures that the sqlite-JDBC driver be loaded.
   */
  @VisibleForTesting
  public static void loadJDBCDriver() {
    try {
      // Load the sqlite-JDBC driver using the current class loader.
      Class.forName(JDBC.class.getName());
    } catch (final ClassNotFoundException why) {
      log.error("Could not load JDBC driver", why);
    }
  }
}
