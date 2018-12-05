/*
 * GNATdashboard
 * Copyright (C) 2016-2018, AdaCore
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

import com.adacore.gnatdashboard.gnathub.api.orm.constant.ResourceKind;
import com.adacore.gnatdashboard.gnathub.api.orm.constant.RuleKind;
import lombok.AllArgsConstructor;
import lombok.Cleanup;
import lombok.SneakyThrows;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

@AllArgsConstructor
public class MeasureDAO {
  private final Connector connector;

  //  File metrics query
  private static final int DEFAULT_METRIC_LINE_NO = 0;
  private static final String SQL = String.join(" ",
      "SELECT",
      "  msg.data as value, rule.identifier as key",
      "FROM",
      "  resources file, resources_messages rm, rules rule, tools tool, messages msg",
      "WHERE",
      "  file.id = rm.resource_id",
      "  AND msg.id = rm.message_id",
      "  AND rule.tool_id = tool.id",
      "  AND msg.rule_id = rule.id",
      "  AND rm.line = ?",
      "  AND file.kind = ?",
      "  AND rule.kind = ?",
      "  AND file.name = ?");


  //  File cyclomatic complexity computation query
  private static final String SQLComplexity = String.join(" ",
          "SELECT",
          "  sum(messages.data) complexity",
          "FROM",
          "  entities, resources, entities_messages, messages, rules",
          "WHERE",
          "  entities.resource_id = resources.id",
          "  AND entities_messages.entity_id = entities.id",
          "  AND messages.id = entities_messages.message_id",
          "  AND messages.rule_id = rules.id",
          "  AND rules.identifier = \"cyclomatic_complexity\"",
          "  AND resources.name = ?");

  //  File package level LSLOC computation query
  private static final String SQLPckgLSLOC = String.join(" ",
          "SELECT",
          "  messages.data as value",
          "FROM",
          "  entities, resources, entities_messages, messages, rules",
          "WHERE",
          "  entities.resource_id = resources.id",
          "  AND entities_messages.entity_id = entities.id",
          "  AND messages.id = entities_messages.message_id",
          "  AND messages.rule_id = rules.id",
          "  AND entities.kind = \"compilation unit\"",
          "  AND rules.identifier = \"lsloc\"",
          "  AND resources.name = ?");

  //  File package level all_stmts computation query
  private static final String SQLPckgAllStmts = String.join(" ",
          "SELECT",
          "  messages.data as value",
          "FROM",
          "  entities, resources, entities_messages, messages, rules",
          "WHERE",
          "  entities.resource_id = resources.id",
          "  AND entities_messages.entity_id = entities.id",
          "  AND messages.id = entities_messages.message_id",
          "  AND messages.rule_id = rules.id",
          "  AND entities.kind = \"compilation unit\"",
          "  AND rules.identifier = \"all_stmts\"",
          "  AND resources.name = ?");

  /**
   * Fetch metrics associated to a file.
   *
   * @param path The path to the file.
   * @return The metrics for that file.
   */
  @SneakyThrows
  public final FileMeasures getMeasuresForFile(final String path) {
    //  Execute SQL query on DB to get the complexity on file from
    //  individual cyclomatic complexities of methods
    @Cleanup final PreparedStatement statementComplexity = connector.createStatement(SQLComplexity);
    statementComplexity.setString(1, path);
    ResultSet rsComplexity = statementComplexity.executeQuery();
    Integer complexityVal = rsComplexity.getInt("complexity");

    Integer lslocVal = 0;
    Integer allstmtsVal = 0;

    //  Execute SQL query on DB to get the lsloc metric on package entity
    @Cleanup final PreparedStatement statementLSLOC = connector.createStatement(SQLPckgLSLOC);
    statementLSLOC.setString(1, path);
    ResultSet rsLSLOC = statementLSLOC.executeQuery();
    if (rsLSLOC.next()) {
      lslocVal = rsLSLOC.getInt("value");
    }

    //  Execute SQL query on DB to get the all_stmts metric on package entity
    @Cleanup final PreparedStatement statementALLSTMTS = connector.createStatement(SQLPckgAllStmts);
    statementALLSTMTS.setString(1, path);
    ResultSet rsALLSTMTS = statementALLSTMTS.executeQuery();
    if (rsALLSTMTS.next()) {
      allstmtsVal = rsALLSTMTS.getInt("value");
    }

    //  Fetch and store metrics associated to the file
    @Cleanup final PreparedStatement statement = connector.createStatement(SQL);
    statement.setInt(1, DEFAULT_METRIC_LINE_NO);
    statement.setInt(2, ResourceKind.FILE.img);
    statement.setInt(3, RuleKind.MEASURE.img);
    statement.setString(4, path);

    return new FileMeasures(
            path, complexityVal, lslocVal, allstmtsVal,
            connector.query(
                    statement,
                    resultSet -> new Measure(resultSet.getString("key"), resultSet.getString("value"))));
  }
}
