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

import com.adacore.gnatdashboard.gnathub.api.orm.constant.ResourceKind;
import com.adacore.gnatdashboard.gnathub.api.orm.constant.RuleKind;
import lombok.AllArgsConstructor;
import lombok.Cleanup;
import lombok.SneakyThrows;

import java.sql.PreparedStatement;

@AllArgsConstructor
public class MeasureDAO {
  private final Connector connector;

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

  /**
   * Fetch metrics associated to a file.
   *
   * @param path The path to the file.
   * @return The metrics for that file.
   */
  @SneakyThrows
  public final FileMeasures getMeasuresForFile(final String path) {
    @Cleanup final PreparedStatement statement = connector.createStatement(SQL);
    statement.setInt(1, DEFAULT_METRIC_LINE_NO);
    statement.setInt(2, ResourceKind.FILE.img);
    statement.setInt(3, RuleKind.MEASURE.img);
    statement.setString(4, path);

    return new FileMeasures(path, connector.query(statement,
        resultSet -> new Measure(resultSet.getString("key"), resultSet.getString("value"))));
  }
}
