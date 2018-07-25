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

import com.adacore.gnatdashboard.gnathub.api.orm.constant.RuleKind;
import lombok.AllArgsConstructor;
import lombok.Cleanup;
import lombok.SneakyThrows;

import java.sql.PreparedStatement;

@AllArgsConstructor
public class IssueDAO {
  private final Connector connector;

  private static final String CODEPEER_TOOL = "codepeer";
  private static final String CODEPEER_ANNOTATION_CATEGORY = "annotation";

   // Returns the category string related to the current ranking value
   //
   private String getCategory (int Ranking) {
       if      (Ranking == 0) return "annotation";
       else if (Ranking == 2) return "info";
       else if (Ranking == 3) return "low";
       else if (Ranking == 4) return "medium";
       else if (Ranking == 5) return "high";
       else return "unspecified";
   }

  private static final String SQL = String.join(" ",
      "SELECT",
      "  rm.line as line_no, rule.identifier as key, msg.data as message, msg.ranking as category,",
      "  tool.name as tool_name",
      "FROM",
      "  resources_messages rm, rules rule, tools tool, messages msg, resources file",
      "WHERE",
      "  msg.rule_id = rule.id",
      "  AND rm.message_id = msg.id",
      "  AND rule.tool_id = tool.id",
      "  AND rm.resource_id = file.id",
      "  AND rule.kind = ?",
      "  AND file.name = ?",
      "  AND NOT (tool.name = ? AND msg.ranking = ?)");

  /**
   * Fetch issues associated to a file.
   *
   * @param path The path to the file.
   * @return The issues for that file.
   */
  @SneakyThrows
  public final FileIssues getIssuesForFile(final String path) {
    @Cleanup final PreparedStatement statement = connector.createStatement(SQL);
    statement.setInt(1, RuleKind.ISSUE.img);
    statement.setString(2, path);
    statement.setString(3, CODEPEER_TOOL);
    statement.setString(4, CODEPEER_ANNOTATION_CATEGORY);

    return new FileIssues(path, connector.query(statement,
        resultSet -> new Issue(
            resultSet.getInt("line_no"), resultSet.getString("key"),
            resultSet.getString("tool_name"), resultSet.getString("message"),
            getCategory(resultSet.getInt("category")))));
  }
}
