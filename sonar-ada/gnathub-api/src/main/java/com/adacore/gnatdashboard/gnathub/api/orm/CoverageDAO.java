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
import lombok.extern.slf4j.Slf4j;

import java.sql.SQLException;

@Slf4j
@AllArgsConstructor
public class CoverageDAO {
  private final Connector connector;

  private final static String GNATHUB_COVERAGE_RULE = "coverage";
  private final static String BASE_COVERAGE_QUERY = String.join(" ",
      "SELECT",
      "  file.name as path, rm.line as line, msg.data as hits",
      "FROM",
      "  resources file, resources_messages rm, messages msg, tools tool, rules rule",
      "WHERE",
      "  tool.id = rule.tool_id",
      "    AND rule.id = msg.rule_id",
      "    AND msg.id = rm.message_id",
      "    AND rm.line != 0",
      "    AND rm.resource_id = file.id",
      String.format("AND rule.name = '%s'", GNATHUB_COVERAGE_RULE));

  private final static String COVERAGE_PER_FILE =
      String.join(" ", BASE_COVERAGE_QUERY, "AND file.name = '%s'");

  /**
   * Fetches coverage results for a file.
   *
   * @param path The path to the file.
   * @return The coverage results for that file.
   */
  public final FileCoverage getCoverageForFile(final String path) throws SQLException {
    return new FileCoverage(path, connector.query(String.format(COVERAGE_PER_FILE, path),
        resultSet -> new LineHits(resultSet.getInt("line"), resultSet.getInt("hits"))));
  }
}
