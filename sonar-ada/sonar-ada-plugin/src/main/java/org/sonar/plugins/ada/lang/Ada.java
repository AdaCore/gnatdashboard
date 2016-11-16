/*
 * Sonar Ada Plugin (GNATdashboard)
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

package org.sonar.plugins.ada.lang;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.sonar.api.config.Settings;
import org.sonar.api.resources.AbstractLanguage;
import org.sonar.plugins.ada.AdaPlugin;

import java.util.List;

/**
 * Defines a new language for SonarQube.
 *
 * Provides the basic of the language to the SonarQube core engine.
 */
@Slf4j
public class Ada extends AbstractLanguage {
  public static final String NAME = "Ada";
  public static final String KEY = "ada";

  public static final String DEFAULT_FILE_SUFFIXES = "adb,ads,ada";

  private final Settings settings;

  /**
   * Default constructor
   */
  public Ada(Settings settings) {
    super(KEY, NAME);
    this.settings = settings;
  }

  /**
   * Only for testing purposes
   */
  @VisibleForTesting
  public Ada() {
    this(new Settings());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String[] getFileSuffixes() {
    String[] suffixes = filterEmptyStrings(
        settings.getStringArray(AdaPlugin.FILE_SUFFIXES_KEY));

    if (suffixes.length == 0) {
      suffixes = StringUtils.split(Ada.DEFAULT_FILE_SUFFIXES, ",");
    }

    log.debug("Ada file suffixes:");
    for (final String suffix : suffixes) {
      log.debug(" + {}", suffix);
    }

    return suffixes;
  }

  /**
   * Removes empty strings from the array.
   *
   * @param stringArray The original array of string.
   * @return A new array list containing the only non-blank element of the
   * input array.
   */
  private String[] filterEmptyStrings(String[] stringArray) {
    final List<String> nonEmptyStrings = Lists.newArrayList();

    for (final String string : stringArray) {
      final String trimmed = string.trim();

      if (StringUtils.isNotBlank(trimmed)) {
        nonEmptyStrings.add(trimmed);
      }
    }

    return nonEmptyStrings.toArray(new String[nonEmptyStrings.size()]);
  }
}
