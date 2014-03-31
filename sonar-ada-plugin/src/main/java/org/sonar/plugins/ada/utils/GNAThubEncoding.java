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

package org.sonar.plugins.ada.utils;

import org.sonar.plugins.ada.codepeer.CodePeerSeverity;

public final class GNAThubEncoding {
  private static final String ENCODED_RULE_KEY_SEP = "__";

  /**
   * Decodes the given category to generate the appropriate rule key.
   *
   * @param key The initial rule key.
   * @param category The additional category field that convey more
   *    information about the rule.
   * @return The rule key to use.
   */
  public static String toSonarRuleKey(final String key, final String category) {
    if (category == null) {
      return key;
    }

    final String[] parts = category.split(ENCODED_RULE_KEY_SEP, 2);
    if (parts.length != 2) {
      return String.format("%s%s%s", category, ENCODED_RULE_KEY_SEP, key);
    }

    final CodePeerSeverity severity = CodePeerSeverity.valueOf(parts[0]);
    return String.format("%s%s%s%s%s", severity.getSonarSeverity(),
        ENCODED_RULE_KEY_SEP, parts[1], ENCODED_RULE_KEY_SEP, key);
  }

  /**
   * Decodes the given Sonar rule key to extract the following three
   * information:
   *    - The initial key
   *    - The severity (if any)
   *    - The category
   *
   * @param key The Sonar rule key.
   * @return An array of maximum 3 strings.
   */
  public static String[] fromSonarRuleKey(final String key) {
    return key.split(ENCODED_RULE_KEY_SEP, 3);
  }
}
