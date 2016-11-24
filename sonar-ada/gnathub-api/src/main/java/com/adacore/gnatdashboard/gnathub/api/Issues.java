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

import com.adacore.gnatdashboard.gnathub.api.orm.Connector;
import com.adacore.gnatdashboard.gnathub.api.orm.FileIssues;
import com.adacore.gnatdashboard.gnathub.api.orm.IssueDAO;
import lombok.AllArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@AllArgsConstructor
public class Issues {
  final private Connector connector;
  final private SourceMapper srcMapper;

  /**
   * Return the coverage information GNAThub collected for a file.
   *
   * @param path The absolute path of the source file.
   * @return The coverage information collected by GNAThub, or {@code null} if no coverage found.
   */
  @SneakyThrows
  public FileIssues forFile(final String path) {
    final String originalPath = srcMapper.getOriginalPath(path);
    return originalPath == null ? null : new IssueDAO(connector).getIssuesForFile(originalPath);
  }
}
