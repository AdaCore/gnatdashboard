/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import antlr.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Resource;

/**
 * A class that represents a Ada directory in Sonar
 *
 */
public class AdaDirectory extends Resource<AdaProject> {

    private AdaProject parent;
    private String project;

    /**
     * Creates a AdaDirectory from its key.
     */
    public AdaDirectory(String directoryName) {
//        if (project == null || directoryName == null) {
//            throw new IllegalArgumentException("Ada project and directory name can not be null");
//        }
        setKey(directoryName);
        this.project = project;
        this. parent = null;
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
        return Resource.SCOPE_SPACE;
    }

    @Override
    public String getQualifier() {
        return Resource.QUALIFIER_DIRECTORY;
    }

    @Override
    public AdaProject getParent() {
//        if (parent == null) {
//            parent = new AdaProject(project);
//        }
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
