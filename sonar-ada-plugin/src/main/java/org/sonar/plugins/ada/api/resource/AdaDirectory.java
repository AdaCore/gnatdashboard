/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.api.resource;

import org.sonar.plugins.ada.api.resource.AdaProject;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Qualifiers;
import org.sonar.api.resources.Resource;
import org.sonar.api.resources.Scopes;
import org.sonar.plugins.ada.Ada;

/**
 * A class that represents a Ada directory in Sonar
 *
 */
public class AdaDirectory extends Resource<AdaProject> {

    private AdaProject parent;
    private String project;

    /**
     * Creates a Ada Directory from its name and parent project name.
     */
    public AdaDirectory(String directoryName, String project) {
        setKey(directoryName);
        this.project = project;
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
        return Scopes.DIRECTORY;
    }

    @Override
    public String getQualifier() {
        return Qualifiers.PACKAGE;
    }

    /**
     * Ada directory's parent is an Ada Project
     * Creates the parent if it is null
     *
     * @return AdaProject
     */
    @Override
    public AdaProject getParent() {
        if (parent == null) {
            parent = new AdaProject(project);
        }
        return parent;
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
