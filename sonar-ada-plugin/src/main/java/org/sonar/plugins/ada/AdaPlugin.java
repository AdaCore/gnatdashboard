/****************************************************************************
 *                              Sonar Ada Plugin                            *
 *                                                                          *
 *                     Copyright (C) 2013-2014, AdaCore                     *
 *                                                                          *
 * This is free software;  you can redistribute it  and/or modify it  under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  This software is distributed in the hope  that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 ****************************************************************************/

package org.sonar.plugins.ada;

import com.google.common.collect.ImmutableList;
import lombok.ToString;
import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;
import org.sonar.plugins.ada.codepeer.CodePeerRuleRepository;
import org.sonar.plugins.ada.codepeer.CodePeerMetrics;
import org.sonar.plugins.ada.codepeer.CodePeerViolationsDecorator;
import org.sonar.plugins.ada.gnat.*;
import org.sonar.plugins.ada.gnu.GcovSensor;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.lang.AdaColorizer;
import org.sonar.plugins.ada.ui.CodePeerViolationsRubyWidget;
import org.sonar.plugins.ada.ui.GNATcheckViolationsRubyWidget;

import java.util.List;

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
   * Gets the extensions.
   *
   * @return the extensions
   * @see org.sonar.api.SonarPlugin#getExtensions()
   */
  @Override
  public List getExtensions() {
    return ImmutableList.of(
        Ada.class,
        AdaProjectContext.class,

        AdaSourceImporter.class,
        AdaDefaultProfile.class,
        AdaColorizer.class,

        // Issues
        AdaIssueSensor.class,

        // CodeePeer
        CodePeerRuleRepository.class,
        CodePeerMetrics.class,
        CodePeerViolationsRubyWidget.class,
        CodePeerViolationsDecorator.class,

        // GNATcheck
        GNATcheckRuleRepository.class,
        GNATcheckMetrics.class,
        GNATcheckViolationsRubyWidget.class,
        GNATcheckViolationsDecorator.class,
        GNATcheckViolationsDensityDecorator.class,
        GNATcheckWeightedViolationsDecorator.class,

        // GNATmetric
        GNATmetricSensor.class,
        GNATmetricMetrics.class,

        // Gcov
        GcovSensor.class,

        // GNATprove
        GNATproveRuleRepository.class);
  }
}
