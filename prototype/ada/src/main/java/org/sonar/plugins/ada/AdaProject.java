/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import org.apache.commons.lang.StringUtils;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Resource;

public class AdaProject extends Resource {


    public AdaProject(String projectName) {
        if (projectName == null) {
            throw new IllegalArgumentException("Ada project name can not be null");
        }
        setKey(projectName);
    }

    @Override
    public String getName() {
        return getKey();
    }

    @Override
    public String getLongName() {
        return getKey();
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Override
    public Language getLanguage() {
        return Ada.INSTANCE;
    }

    @Override
    public String getScope() {
        return Resource.SCOPE_SET;
    }

    @Override
    public String getQualifier() {
        return Resource.QUALIFIER_PROJECT;
    }

    @Override
    public AdaProject getParent() {
        return null;
    }

    @Override
    public boolean matchFilePattern(String string) {
        return false;
    }
}
