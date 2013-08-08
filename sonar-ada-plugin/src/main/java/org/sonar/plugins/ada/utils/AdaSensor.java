/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.utils;

import org.sonar.api.batch.Sensor;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.Ada;

/**
 * Generic Sensor for external tool, retrieve informations from reports.
 */
public abstract class AdaSensor implements Sensor {

    /**
     * This sensor is executed only for Ada projects.
     */
    @Override
    public boolean shouldExecuteOnProject(Project project) {
        return Ada.KEY.equals(project.getLanguageKey());
    }

}
