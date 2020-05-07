/*
 * GNATdashboard
 * Copyright (C) 2016 - 2020, AdaCore
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

import com.adacore.gnatdashboard.gnathub.api.Coverage;
import com.adacore.gnatdashboard.gnathub.api.Issues;
import com.adacore.gnatdashboard.gnathub.api.Measures;
import com.adacore.gnatdashboard.gnathub.api.SourceMapper;
import com.adacore.gnatdashboard.gnathub.api.orm.Connector;
import lombok.Getter;
import org.sonar.api.scanner.ScannerSide;
import org.sonar.api.config.Configuration;
import org.sonar.squidbridge.api.AnalysisException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

@ScannerSide
public class GNAThub {
  @Getter private final Connector connector;
  @Getter private final Coverage coverage;
  @Getter private final Measures measures;
  @Getter private final Issues issues;

  public GNAThub(final Configuration config) {
    final String dbUri = config.get(AdaPlugin.GNATHUB_DB_KEY).orElse(null);
    final String srcMappingUri = config.get(AdaPlugin.GNATHUB_SRC_MAPPING_KEY).orElse(null);

    if (dbUri == null) {
      throw new AnalysisException(String.format(
          "%s is not defined in the project properties file", AdaPlugin.GNATHUB_DB_KEY));
    }

    if (srcMappingUri == null) {
      throw new AnalysisException(String.format(
          "%s is not defined in the project properties file", AdaPlugin.GNATHUB_SRC_MAPPING_KEY));
    }

    final Properties mapping = new Properties();
    try {
        mapping.load(new FileInputStream(srcMappingUri));
    } catch (final FileNotFoundException why) {
      throw new AnalysisException("Cannot load source file mapping", why);
    } catch (final IOException why) {
      throw new AnalysisException("Error reading source file mapping", why);
    }

    final SourceMapper srcMapper = new SourceMapper(mapping);

    this.connector = new Connector(dbUri);
    this.coverage = new Coverage(connector, srcMapper);
    this.measures = new Measures(connector, srcMapper);
    this.issues = new Issues(connector, srcMapper);
  }
}
