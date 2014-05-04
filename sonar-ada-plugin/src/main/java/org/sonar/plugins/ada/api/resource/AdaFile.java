/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.api.resource;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.Ada;

/**
 * A class that represents an Ada source file.
 *
 */
public class AdaFile extends Resource<AdaDirectory> {

    private AdaDirectory parent;
    private String project;
    private String directory;
    private String fileName;
    public static final String SEPARATOR = "/";
    public static final String DEFAULT_PROJECT_NAME = "Default project";

    // Used by MyBatis
    public AdaFile (){
    }

    public AdaFile(String sourceName, String prj, String dir) {
        if (prj == null) {
            throw new IllegalArgumentException("Ada source's project name can not be null");
        }
        if (dir == null) {
            throw new IllegalArgumentException("Ada source's directory name can not be null");
        }
        this.project = prj;
        this.directory = dir;
        this.fileName = StringUtils.substringAfterLast(sourceName, SEPARATOR);
        setKey(sourceName);

    }

     public void setProject(String project) {
        this.project = project;
    }

    public void setDirectory(String directory) {
        this.directory = directory;
    }

    public void setFileName(String fileName) {
        setKey(fileName);
        this.fileName = StringUtils.substringAfterLast(fileName, SEPARATOR);
    }

    /**
     * @return resource's base name
     */
    @Override
    public String getName() {
        return fileName;
    }

    /**
     * @return resource's absolute path
     */
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
        return Resource.SCOPE_ENTITY;
    }

    @Override
    public String getQualifier() {
        return Resource.QUALIFIER_FILE;
    }

    /**
     * Ada file's parent is an Ada directory
     * @return AdaDirectory
     */
    @Override
    public AdaDirectory getParent() {
        if (parent == null) {
            parent = new AdaDirectory(directory, project);
        }
        return parent;
    }

    @Override
    public boolean matchFilePattern(String string) {
        //As default value but needs investigation
        return false;
    }

    public static AdaFile fromIOFile(java.io.File file, String prj, String dir) {
         if (file != null) {
            return new AdaFile(file.getPath(), prj, dir);
        }
        return null;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("key", getKey()).append("dir", directory).append("filename", getKey()).append("language", getLanguage()).toString();
    }
}