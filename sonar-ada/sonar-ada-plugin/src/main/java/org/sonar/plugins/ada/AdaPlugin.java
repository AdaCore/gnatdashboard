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

package org.sonar.plugins.ada;

import org.sonar.api.Plugin;
import org.sonar.api.config.PropertyDefinition;
import org.sonar.api.resources.Qualifiers;
import org.sonar.plugins.ada.rules.CodePeerRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.GNATcheckRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.GNATcoverageRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.sensors.GNAThubIssueSensor;
import org.sonar.plugins.ada.sensors.GNATmetricSensor;
import org.sonar.plugins.ada.sensors.GNAThubCoverageSensor;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.lang.AdaColorizer;

/**
 * Entry point to the SonarQube's plug-in.
 *
 * Lists all extensions to SonarQube defined and implemented in this plug-in.
 */
public final class AdaPlugin implements Plugin {

  static final String GNATHUB_DB_KEY = "sonar.ada.gnathub.db";
  static final String GNATHUB_SRC_MAPPING_KEY = "sonar.ada.gnathub.src_mapping";
  public static final String FILE_SUFFIXES_KEY = "sonar.ada.file.suffixes";

  private static final String ADA_CATEGORY = "Ada";
  private static final String GENERAL_CATEGORY = "General";
  private static final String GNATHUB_CATEGORY = "GNAThub";

  /**
   * Initialize SonarQube plugin for the Ada language.
   */
  @Override
  public void define(final Context context) {
    context.addExtensions(
        // Declare the Ada language
        Ada.class,
        AdaColorizer.class,
        AdaDefaultProfile.class,
        AdaProjectContext.class,

        // Register custom metrics
        AdaMetrics.class,

        // Register tools rules
        CodePeerRulesDefinitionXmlLoader.class,
        GNATcheckRulesDefinitionXmlLoader.class,
        GNATcoverageRulesDefinitionXmlLoader.class,

        // Collect metrics and issues
        GNAThubIssueSensor.class,
        GNAThubCoverageSensor.class,
        GNATmetricSensor.class,

        // Compute higher level metrics
        AdaCountIssuesDecorator.class,

        PropertyDefinition.builder(FILE_SUFFIXES_KEY)
            .defaultValue(Ada.DEFAULT_FILE_SUFFIXES)
            .name("File Suffixes")
            .description("Comma-separated list of suffixes of Ada files")
            .onQualifiers(Qualifiers.MODULE, Qualifiers.PROJECT)
            .category(ADA_CATEGORY)
            .subCategory(GENERAL_CATEGORY)
            .build(),

        PropertyDefinition.builder(GNATHUB_DB_KEY)
            .name("Path to GNAThub database")
            .description("GNAThub DB full path")
            .onQualifiers(Qualifiers.MODULE, Qualifiers.PROJECT)
            .category(ADA_CATEGORY)
            .subCategory(GNATHUB_CATEGORY)
            .build(),

        PropertyDefinition.builder(GNATHUB_SRC_MAPPING_KEY)
            .name("Path to GNAThub source mapping file")
            .description("GNAThub source mapping file full path")
            .onQualifiers(Qualifiers.MODULE, Qualifiers.PROJECT)
            .category(ADA_CATEGORY)
            .subCategory(GNATHUB_CATEGORY)
            .build()
    );
  }
}
