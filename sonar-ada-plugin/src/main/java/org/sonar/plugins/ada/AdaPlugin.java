/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2013-2015, AdaCore
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

package org.sonar.plugins.ada;

import com.google.common.collect.ImmutableList;
import lombok.ToString;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.plugins.ada.codepeer.CodePeerRulesDefinition;
import org.sonar.plugins.ada.coverage.CoverageSensor;
import org.sonar.plugins.ada.gnat.*;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.lang.AdaColorizer;

import java.util.List;

/**
 * Entry point to the SonarQube's plug-in.
 *
 * Lists all extensions to SonarQube defined and implemented in this plug-in.
 */
@ToString
@Properties({
    @Property(
        key = AdaPlugin.FILE_SUFFIXES_KEY,
        defaultValue = Ada.DEFAULT_FILE_SUFFIXES,
        name = "File Suffixes",
        description = "Comma-separated list of suffixes of Ada files to analyze.",
        global = true,
        project = true),
    @Property(
        key = AdaPlugin.GNATHUB_DB_KEY,
        name = "Path to GNAThub database",
        description = "GNAThub DB full path",
        global = false,
        project = true),
    @Property(
        key = AdaPlugin.GNATHUB_SRC_MAPPING_KEY,
        name = "Path to GNAThub source mapping file",
        description = "GNAThub source mapping file full path",
        global = false,
        project = true)
})
public final class AdaPlugin extends SonarPlugin {

  public static final String GNATHUB_DB_KEY = "sonar.ada.gnathub.db";
  public static final String GNATHUB_SRC_MAPPING_KEY =
      "sonar.ada.gnathub.src_mapping";
  public static final String FILE_SUFFIXES_KEY = "sonar.ada.file.suffixes";

  /**
   * Lists SonarQube extensions for the Ada language.
   *
   * @return the sonar-ada-plugin extensions.
   * @see org.sonar.api.SonarPlugin#getExtensions()
   */
  @Override
  public List getExtensions() {
      return ImmutableList.of(
              // Declare the Ada language
              Ada.class,
              AdaColorizer.class,
              AdaDefaultProfile.class,
              AdaProjectContext.class,

              // Register custom metrics
              AdaMetrics.class,

              // Register tools rules
              CodePeerRulesDefinition.class,
              GNATcheckRulesDefinition.class,
              GNATcoverageRulesDefinition.class,
              GNATproveRulesDefinition.class,

              // Collect metrics and issues
              GNAThubIssueSensor.class,
              CoverageSensor.class,
              GNATmetricSensor.class,

              // Compute higher level metrics
              AdaCountIssuesDecorator.class
      );
  }
}
