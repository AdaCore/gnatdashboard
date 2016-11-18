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

package org.sonar.plugins.ada.utils;

import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Resource;
import org.sonar.api.resources.Scopes;

import java.util.ArrayList;
import java.util.Collection;

@Deprecated
public class ResourceUtils {
  /**
   * Expand the list of child resources.
   *
   * If resource is a {@code File}, the result is list containing a single
   * {@code file}. If resource is a {@code Directory} or a {@code Project},
   * the result is the list of child files, fetched recursively.
   *
   * @param resource The resource to expand.
   * @param context  The sensor context.
   * @return The list of {@code File}s.
   */
  public static Collection<Resource> expandChildren(
      final Resource resource, final SensorContext context) {
    Collection<Resource> files = new ArrayList<Resource>();
    if (resource.getScope().equals(Scopes.FILE)) {
      files.add(resource);
    } else {
      assert resource.getScope().equals(Scopes.DIRECTORY) ||
          resource.getScope().equals(Scopes.PROJECT);
      for (final Resource child : context.getChildren(resource)) {
        files.addAll(expandChildren(child, context));
      }
    }
    return files;
  }
}
