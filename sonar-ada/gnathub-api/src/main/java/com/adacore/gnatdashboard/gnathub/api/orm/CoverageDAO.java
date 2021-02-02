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
import lombok.SneakyThrows;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

@AllArgsConstructor
public class CoverageDAO {
  private final Connector connector;

  public enum CoverageInfo {
    NOT_COVERED,
    PARTIALLY_COVERED,
    COVERED,
    NO_CODE,
    NOT_COVERABLE,
    EXEMPTED_WITH_VIOLATION,
    EXEMPTED_NO_VIOLATION
  }

  private static final String GNATCOVERAGE_TOOL_ID = "1";
  private static final String GNATHUB_COVERAGE_RULE = "coverage";
  private static final String SQL_COVERAGE = String.join(" ",
          "SELECT",
          "  rm.line as line, msg.data as hits, tool.name as toolname",
          "FROM",
          "  resources file, resources_messages rm, messages msg, tools tool, rules rule",
          "WHERE",
          "  tool.id = ?",
          "  AND rule.id = msg.rule_id",
          "  AND msg.id = rm.message_id",
          "  AND rm.line != 0",
          "  AND rm.resource_id = file.id",
          "  AND rule.name = ?",
          "  AND file.name = ?");

  // To know which tool was used to compute data coverage.
  // If we have results for both tools, we will prioritize GNATcoverage.
  private static final String SQL_TOOL = String.join(" ",
          "SELECT DISTINCT",
          "  rule.tool_id",
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

  // Know if partial coverage is due to decision coverage or MC/DC.
  // In the latter, we will mark the line as covered, and emit the MC/DC violation
  // as a message info. If Sonar add coverage semantics in the future, this will need
  // to be reconsidered.
  private static final String SQL_DECISION_MESSAGE = String.join(" ",
          "SELECT",
          "  msg.data",
          "FROM",
          "  resources file, resources_messages rm, messages msg, rules rule",
          "WHERE ",
          "  rule.id = msg.rule_id",
          "  AND msg.id = rm.message_id",
          "  AND rm.line = ?",
          "  AND rm.resource_id = file.id",
          "  AND rule.name = \"decision\"",
          "  AND file.name = ?");

  /**
   * Fetch coverage results for a file.
   *
   * @param path The path to the file.
   * @return The coverage results for that file.
   */
  @SneakyThrows
  public final FileCoverage getCoverageForFile(final String path) {
    @Cleanup final PreparedStatement toolStmt = connector.createStatement(SQL_TOOL);
    toolStmt.setString(1, GNATHUB_COVERAGE_RULE);
    toolStmt.setString(2, path);
    ResultSet tools = toolStmt.executeQuery();
    String tool_id = null;
    while (tools.next()){
      tool_id = tools.getString(1);

      // GNATcoverage data for this file. We will prioritize it over gcov.
      if (tool_id.equals(GNATCOVERAGE_TOOL_ID)){
        break;
      }
    }
    // No coverage for this file with both tools
    if (tool_id == null){
      return null;
    }

    @Cleanup final PreparedStatement statement = connector.createStatement(SQL_COVERAGE);
    statement.setString(1, tool_id);
    statement.setString(2, GNATHUB_COVERAGE_RULE);
    statement.setString(3, path);

    RowMapper<LineHits> lambda = (resultSet) ->
    {
      if (resultSet.getString("toolname").equals("gnatcoverage")) {
        CoverageInfo covInfo = CoverageInfo.valueOf(resultSet.getString("hits"));
        int line = resultSet.getInt("line");
        int count = 0;
        int conditions = 0;
        int conditionsCovered = 0;
        switch (covInfo) {
          case NOT_COVERED:
            count = 0;
            break;
          case PARTIALLY_COVERED:
            count = 1;
            conditions = 2;
            @Cleanup final PreparedStatement decisionStmt = connector.createStatement(SQL_DECISION_MESSAGE);
            decisionStmt.setInt(1, line);
            decisionStmt.setString(2, path);
            ResultSet decisionMsg = decisionStmt.executeQuery();
            if (decisionMsg.next()){
              // Partial coverage is due to decision coverage
              conditionsCovered = 1;
            }
            else {
              // Partial coverage is due to MC/DC
              conditionsCovered = 2;
            }
            break;
          case COVERED:
            count = 1;
            break;
          default:
            // NO_CODE, NOT_COVERABLE, EXEMPTED_WITH_VIOLATION, EXEMPTED_NO_VIOLATION will
            // be considered as NO_CODE in Sonar as there are no semantics defined for it (in Sonar).
            return null;
        }
        return new LineHits(line, count, conditions, conditionsCovered, "gnatcoverage");
      }
      else {
        // tool is gcov
        return new LineHits(resultSet.getInt("line"), resultSet.getInt("hits"), "gcov");
      }
    };
    FileCoverage coverage = new FileCoverage(path, connector.query(statement,
            lambda));
    return coverage;
  }
}
