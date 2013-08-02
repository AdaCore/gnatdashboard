/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.api.resource;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Qualifiers;
import org.sonar.api.resources.Resource;
import org.sonar.api.resources.Scopes;
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
        return Scopes.PROJECT;
    }

    @Override
    public String getQualifier() {
        return Qualifiers.MODULE;
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
