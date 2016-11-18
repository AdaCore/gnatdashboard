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

package com.adacore.gnatdashboard.gnathub.api;

import lombok.extern.slf4j.Slf4j;

import java.util.Properties;

@Slf4j
public class SourceMapper {
  private final Properties srcMapping;
  private final Properties reverseSrcMapping;

  public SourceMapper(final Properties srcMapping) {
    this.srcMapping = srcMapping;

    // Generate the reverse property mapping to allow queries both ways
    this.reverseSrcMapping = new Properties();
    for (final Object key : srcMapping.keySet()) {
      reverseSrcMapping.put(srcMapping.get(key), key);
    }
  }

  /**
   * Returns the path to the file.
   *
   * Use source mapping to compute the local cached file path. Returns {@code null} if not found.
   *
   * @param originalPath The original path for the file.
   * @return The mapped path, or {@code null} if not mapped.
   */
  public String getAnalysisPath(final String originalPath) {
    if (!srcMapping.containsKey(originalPath)) {
      log.warn("No source mapping found for: {}", originalPath);
      return null;
    }

    return srcMapping.getProperty(originalPath);
  }

  /**
   * Returns the path to the file.
   *
   * Use reverse source mapping to compute the original file path. Returns {@code null} if not
   * found.
   *
   * @param analysisPath The original path for the file.
   * @return The mapped path, or {@code null} if not mapped.
   */
  public String getOriginalPath(final String analysisPath) {
    if (!reverseSrcMapping.containsKey(analysisPath)) {
      log.warn("No reverse source mapping found for: {}", analysisPath);
      return null;
    }

    return reverseSrcMapping.getProperty(analysisPath);
  }
}
