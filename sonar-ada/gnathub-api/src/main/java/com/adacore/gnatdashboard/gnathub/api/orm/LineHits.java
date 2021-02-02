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
import lombok.ToString;

@ToString
public class LineHits {
  public final int line;

  // gcov provides the number of test that hit a particular line,
  // but not GNATcoverage.
  public final int count;

  // We will use Sonar condition coverage to express decision coverage
  public final int conditions;
  public final int conditionsCovered;

  // To know if data was computed by gcov or GNATcoverage
  public String toolname;

  // Constructor for gcov data
  public LineHits (int line, int count, String toolname){
    this.line = line;
    this.count = count;
    this.conditions = 0;
    this.conditionsCovered = 0;
    this.toolname = toolname;
  }

  // Constructor for GNATcoverage data
  public LineHits (int line, int count, int conditions, int conditionsCovered, String toolname) {
    this.line = line;
    this.count = count;
    this.conditions = conditions;
    this.conditionsCovered = conditionsCovered;
    this.toolname = toolname;
  }

}
