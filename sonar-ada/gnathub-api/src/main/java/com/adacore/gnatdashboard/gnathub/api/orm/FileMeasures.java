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

import lombok.AllArgsConstructor;
import lombok.experimental.Delegate;

import java.util.List;

@AllArgsConstructor
public class FileMeasures {
  public final String path;
  public final int complexity;
  public final int GNATmetricPckgLSLOC;
  public final int GNATmetricPckgAllStmts;

  @Delegate public final Measures measures;

  public FileMeasures(final String path,
                      final Integer complexity,
                      final Integer GNATmetricPckgLSLOC,
                      final Integer GNATmetricPckgAllStmts,
                      final List<Measure> measures)
  {
    this(path, complexity, GNATmetricPckgLSLOC, GNATmetricPckgAllStmts, new Measures(measures));
  }
}
