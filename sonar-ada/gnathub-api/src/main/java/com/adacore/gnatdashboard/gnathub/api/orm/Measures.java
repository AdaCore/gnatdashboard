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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Measures {
  private Map<String, String> measures;

  public Measures(final List<Measure> measures) {
    this.measures = new HashMap<>();
    for (final Measure measure : measures) {
      this.measures.put(measure.getKey(), measure.getRawValue());
    }
  }

  public Integer asInt(final String key) {
    if (!this.measures.containsKey(key)) {
      return null;
    }
    return Integer.valueOf(this.measures.get(key));
  }

  public Double asDouble(final String key) {
    if (!this.measures.containsKey(key)) {
      return null;
    }
    return Double.valueOf(this.measures.get(key));
  }
}
