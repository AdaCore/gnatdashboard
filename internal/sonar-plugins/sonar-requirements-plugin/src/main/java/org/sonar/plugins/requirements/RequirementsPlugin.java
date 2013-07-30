/*
 * Sonar Requirements Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.requirements;

import java.util.ArrayList;
import java.util.List;
import org.sonar.api.Extension;
import org.sonar.api.SonarPlugin;

/**
 * Plugin for following requirements
 */
public class RequirementsPlugin extends SonarPlugin{

    @Override
    public List getExtensions() {
        List<Class<? extends Extension>> l = new ArrayList<Class<? extends Extension>>();

        l.add(RequirementsMetrics.class);
        l.add(RequirementsSensor.class);

        return l;
    }

}
