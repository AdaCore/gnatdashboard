/*
 * Sonar Modeling Plugin
 * Copyright (C) 2013, AdaCore
 */

 package com.adacore.sonar.reference;

import org.sonar.api.Properties;
import org.sonar.api.Property;
import org.sonar.api.SonarPlugin;

import java.util.Arrays;
import java.util.List;

/**
 * This class is the entry point for all extensions
 */
@Properties({})

public final class ModelingPlugin extends SonarPlugin {

  // Sonar extensions
  public List getExtensions() {
    return Arrays.asList(
        // Definitions
        ModelingMetrics.class);
  }
}
