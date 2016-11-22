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
import lombok.extern.slf4j.Slf4j;

import java.sql.SQLException;

@Slf4j
@AllArgsConstructor
public class MeasureDAO {
  private final Connector connector;

  private final static String BASE_MEASURE_QUERY = String.join(" ",
      "SELECT ",
      "  msg.data as value, rule.identifier as key",
      "FROM",
      "  resources file, resources_messages rm, rules rule, tools tool, messages msg",
      "WHERE",
      "  rm.line = 0",
      String.format("    AND file.kind = %d", ResourceKind.FILE.img),
      "    AND file.id = rm.resource_id",
      "    AND msg.id = rm.message_id",
      "    AND rule.tool_id = tool.id",
      "    AND msg.rule_id = rule.id",
      String.format("AND rule.kind = %d", RuleKind.MEASURE.img));

  private final static String MEASURES_PER_FILE =
      String.join(" ", BASE_MEASURE_QUERY, "AND file.name = '%s'");

  /**
   * Fetch metrics associated to a file.
   *
   * @param path The path to the file.
   * @return The metrics for that file.
   */
  public final FileMeasure getMeasuresForFile(final String path) throws SQLException {
    return new FileMeasure(path, connector.query(String.format(MEASURES_PER_FILE, path),
        resultSet -> new Measure(resultSet.getString("key"), resultSet.getString("value"))));
  }
}
