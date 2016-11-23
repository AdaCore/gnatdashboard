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

import com.adacore.gnatdashboard.gnathub.api.orm.constant.RuleKind;
import lombok.AllArgsConstructor;
import lombok.Cleanup;

import java.sql.PreparedStatement;
import java.sql.SQLException;

@AllArgsConstructor
public class IssueDAO {
  private final Connector connector;

  private static final String SQL = String.join(" ",
      "SELECT",
      "  rm.line as line_no, rule.identifier as key, msg.data as message, c.label as category,",
      "  tool.name as tool_name",
      "FROM",
      "  resources_messages rm, rules rule, tools tool, messages msg, resources file",
      "LEFT OUTER JOIN",
      "  categories c ON msg.category_id = c.id",
      "WHERE",
      "  msg.rule_id = rule.id",
      "  AND rm.message_id = msg.id",
      "  AND rule.tool_id = tool.id",
      "  AND rm.resource_id = file.id",
      "  AND rule.kind = ?",
      "  AND file.name = ?");

  /**
   * Fetch issues associated to a file.
   *
   * @param path The path to the file.
   * @return The issues for that file.
   * @throws SQLException
   */
  public final FileIssues getIssuesForFile(final String path) throws SQLException {
    @Cleanup final PreparedStatement statement = connector.createStatement(SQL);
    statement.setInt(1, RuleKind.ISSUE.img);
    statement.setString(2, path);

    return new FileIssues(path, connector.query(statement,
        resultSet -> new Issue(
            resultSet.getInt("line_no"), resultSet.getString("key"),
            resultSet.getString("tool_name"), resultSet.getString("message"),
            resultSet.getString("category"))));
  }
}
