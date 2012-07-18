/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.sonar.api.resources.DefaultProjectFileSystem;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * A class that represents a Ada source file.
 *
 */
public class AdaFile extends Resource<AdaDirectory> {

    private AdaDirectory parent;
    private String project;
    private String directory;
    private String fileName;
    public static final String SEPARATOR = "/";

    /**
     * Creates a AdaFile based on package and file names
     */
    public AdaFile(String sourceName) {
//        if (project == null || directory == null || sourceName == null) {
//            throw new IllegalArgumentException("Ada project, directory and file name can not be null");
//        }
        this.project = project;
        this.directory = StringUtils.substringBeforeLast(sourceName, SEPARATOR);
        this.fileName = StringUtils.substringAfterLast(sourceName, SEPARATOR);
        setKey(sourceName);
        AdaUtils.LOG.info("+++ Creating file: {}", this.directory);
        AdaUtils.LOG.info("With key: {}, with file name: {}", getKey(), fileName);
        AdaUtils.LOG.info("Class of the file: {}", this.getClass());

    }

    @Override
    public String getName() {
        return fileName;
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
        return Resource.SCOPE_ENTITY;
    }

    @Override
    public String getQualifier() {
        return Resource.QUALIFIER_FILE;
    }

    @Override
    public AdaDirectory getParent() {
        if (parent == null) {
            AdaUtils.LOG.info("Creating PARENT");
            parent = new AdaDirectory(directory);
        }
        return parent;
    }

    @Override
    public boolean matchFilePattern(String string) {
        //As default value but needs investigation
        return false;
    }

    /**
     * Creates a File from its name and a project
     */
    public static AdaFile fromIOFile(java.io.File file, List<java.io.File> sourceDirs) {
        String relativePath = DefaultProjectFileSystem.getRelativePath(file, sourceDirs);
        if (relativePath != null) {
            return new AdaFile(relativePath);
        }
        return null;
    }

    public static AdaFile fromIOFile(java.io.File file, Project project) {
        return fromIOFile(file, project.getFileSystem().getSourceDirs());
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("key", getKey()).append("dir", directory).append("filename", getKey()).append("language", getLanguage()).toString();
    }

}
