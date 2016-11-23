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

import lombok.AllArgsConstructor;
import lombok.Cleanup;

import java.sql.PreparedStatement;
import java.sql.SQLException;

@AllArgsConstructor
public class CoverageDAO {
  private final Connector connector;

  private static final String GNATHUB_COVERAGE_RULE = "coverage";
  private static final String SQL = String.join(" ",
      "SELECT",
      "  rm.line as line, msg.data as hits",
      "FROM",
      "  resources file, resources_messages rm, messages msg, tools tool, rules rule",
      "WHERE",
      "  tool.id = rule.tool_id",
      "  AND rule.id = msg.rule_id",
      "  AND msg.id = rm.message_id",
      "  AND rm.line != 0",
      "  AND rm.resource_id = file.id",
      "  AND rule.name = ?",
      "  AND file.name = ?");

  /**
   * Fetch coverage results for a file.
   *
   * @param path The path to the file.
   * @return The coverage results for that file.
   * @throws SQLException
   */
  public final FileCoverage getCoverageForFile(final String path) throws SQLException {
    @Cleanup final PreparedStatement statement = connector.createStatement(SQL);
    statement.setString(1, GNATHUB_COVERAGE_RULE);
    statement.setString(2, path);

    return new FileCoverage(path, connector.query(statement,
        resultSet -> new LineHits(resultSet.getInt("line"), resultSet.getInt("hits"))));
  }
}
