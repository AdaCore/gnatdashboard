/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.resources;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.Ada;

/**
 * A class that represents an Ada sub project.
 *
 */
public class AdaProject extends Resource {

    AdaProject() {
        this(null);
    }

    public AdaProject(String projectName) {
        setKey(projectName);
    }

    @Override
    public String getName() {
        return getKey();
    }

    @Override
    public String getLongName() {
        return null;
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
        return Resource.SCOPE_SPACE;
    }

    @Override
    public String getQualifier() {
        return Resource.QUALIFIER_MODULE;
    }

    /**
     * Ada project has no parent, is attached to the project root
     */
    @Override
    public Resource<?> getParent() {
        return null;
    }

    @Override
    public boolean matchFilePattern(String string) {
        return false;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("id", getId()).append("key", getKey()).toString();
    }
}
