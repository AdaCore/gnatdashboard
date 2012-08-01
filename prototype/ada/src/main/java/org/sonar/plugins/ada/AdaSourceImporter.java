/**
 *  Sonar Ada Plugin
 *  Copyright (C) 2001-2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FileUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.ResourceCreationLock;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.api.resources.Resource;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.resources.AdaFile;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Extension of AbstractSourceImporter for Ada project import all project
 * sources.
 */
@Phase(name = Phase.Name.PRE)
public class AdaSourceImporter extends AbstractSourceImporter {

    // <editor-fold desc="Class's attibutes" defaultstate="collapsed">
    /**
     * Property key of the path to the file that contains the Ada project tree
     * The tree information is retrieved from the Gnat project file
     */
    public static final String PROJECT_TREE_FILE_PATH_KEY = "sonar.ada.projectTree";
    /**
     * Contains the mapping between a resource and its name
     */
    public static Map<String, AdaFile> sourceMap;
    private ResourceCreationLock lock;
    private Configuration config;
    // </editor-fold>

    public AdaSourceImporter(ResourceCreationLock lock, Configuration config) {
        super(Ada.INSTANCE);
        this.lock = lock;
        this.config = config;
    }

    /**
     * {@inheritDoc} Initialize the sourceMap and import sources
     */
    @Override
    public void analyse(Project project, SensorContext context) {
        sourceMap = loadProjectTree(project.getFileSystem(), project.getFileSystem().getBasedir().getPath(), context);
        //analyse(project.getFileSystem(), context);
        saveResource(context, project.getFileSystem().getSourceCharset());
        onFinished();
    }

    /**
     * Import resource content and save it in the context.
     * @param context
     * @param source encoding
     */
    protected void saveResource(SensorContext context, Charset sourcesEncoding) {
        for (AdaFile resource : sourceMap.values()) {
            File file = new File(resource.getLongName());
            try {
                String source = FileUtils.readFileToString(file, sourcesEncoding.name());
                context.saveSource(resource, source);
            } catch (IOException e) {
                throw new SonarException("Unable to read and import the source file : '" + file.getAbsolutePath() + "' with the charset : '"
                        + sourcesEncoding.name() + "'.", e);
            }
        }
    }

    protected Resource createResource(File file, List<File> sourceDirs, boolean unitTest, String project, String directory) {
        return file != null ? AdaFile.fromIOFile(file, sourceDirs, project, directory) : null;
    }

    /**
     * Forbids the creation of resources when saving violations and measures. By
     * default it's unlocked, so only warnings are logged. When locked, then an
     * exception is thrown.
     */
    @Override
    protected void onFinished() {
        lock.lock();
    }

    /**
     * Retrieve Ada project logical tree. As Ada project tree is defined in the
     * project file, it must be retrieved from this file only and not according
     * to the physical directory tree.
     *
     * @param project's file system
     * @param project's base directory path
     * @param sensor's context
     */
    protected Map<String, AdaFile> loadProjectTree(ProjectFileSystem fileSystem, String baseDirPath, SensorContext context) {
        Map<String, AdaFile> srcMap = new HashMap<String, AdaFile>();
        String filePath = config.getString(PROJECT_TREE_FILE_PATH_KEY, null);
        if (filePath == null) {
            AdaUtils.LOG.info("Path to file containing the project tree is missing or invalid, using a default project tree instead.");

        } else {
            AdaUtils.LOG.info("Using pattern '{}' to find project tree file", filePath);

            JSONParser parser = new JSONParser();

            try {

                Object obj = parser.parse(new FileReader(new File(baseDirPath, filePath)));

                JSONObject jsonTree = (JSONObject) obj;
                for (Object project : jsonTree.keySet()) {
                    JSONObject jsonSrcDir = (JSONObject) (jsonTree.get(project));
                    for (Object srcDir : jsonSrcDir.keySet()) {
                        JSONArray jsonSrc = (JSONArray) jsonSrcDir.get(srcDir);
                        Iterator<String> iterator = jsonSrc.iterator();
                        while (iterator.hasNext()) {
                            //For now, import all resources as a source and not as unit test resource.
                            AdaFile resource = (AdaFile) createResource(new File(srcDir.toString(), iterator.next()), fileSystem.getSourceDirs(), false, project.toString(), srcDir.toString());
                            srcMap.put(resource.getName(), resource);
                        }
                    }

                }

            } catch (ParseException ex) {
                AdaUtils.LOG.warn("Error while parsing the Project tree file, using default tree instead.\n {}\n Position {}", ex.getMessage(), ex.getPosition());
            } catch (FileNotFoundException e) {
                AdaUtils.LOG.warn("Connot find the file containing the Project tree, using default tree instead.\n {}", e.getMessage());
            } catch (IOException e) {
                AdaUtils.LOG.warn("Connot open the file containing the Project tree, using default tree instead.\n {}", e.getMessage());
            }
        }
        return srcMap;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
