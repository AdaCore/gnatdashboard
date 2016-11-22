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

/**
 * Generates SQL queries for the Project database.
 */
@Deprecated
public class SQLQueries {
  /**
   * Prevents creation of SQLQueries instances.
   */
  private SQLQueries() {}

  /**
   * Crafts the SQL query to fetch all issues.
   *
   * @return The SQL query to use to fetch all issues from the database.
   */
  public static String allIssues() {
    return BASE_ISSUE_QUERY;
  }

  private final static String BASE_ISSUE_QUERY = String.format(
      "SELECT "
          + "file.name as path, "
          + "msg.data as message, "
          + "rule.identifier as key, "
          + "REPLACE(LOWER(tool.name), ' ', '') as tool_name, "
          + "rm.line as line_no, "
          + "c.label as category "
          + "FROM "
          + "resources_messages rm, rules rule, tools tool, "
          + "messages msg, resources file "
          + "LEFT OUTER JOIN "
          + "categories c ON msg.category_id = c.id "
          + "WHERE "
          + "msg.rule_id = rule.id AND rm.message_id = msg.id "
          + "AND rule.tool_id = tool.id AND rm.resource_id = file.id "
          + "AND rule.kind = %d", RuleKind.ISSUE.img);
}
