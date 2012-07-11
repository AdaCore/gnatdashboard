/**
 *
 * Sonar Ada Plugin
 * Copyright AdaCore, 2012
 */
package org.sonar.plugins.ada.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class AdaUtils {
  
  private AdaUtils() {
    // only static methods
  }

  /**
   * Default logger.
   */
  public static final Logger LOG = LoggerFactory.getLogger("AdaPlugin");
}
