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
import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.ResourceCreationLock;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.api.resources.Resource;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.utils.AdaUtils;

@Phase(name = Phase.Name.PRE)
public class AdaSourceImporter extends AbstractSourceImporter {

    public static final String PROJECT_TREE_FILE_PATH_KEY = "sonar.ada.projectTree";
    public static Map<String, Resource> sourceMap;
    private ResourceCreationLock lock;
    private Configuration config;

    public AdaSourceImporter(ResourceCreationLock lock, Configuration config) {
        super(Ada.INSTANCE);
        this.lock = lock;
        this.config = config;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void analyse(Project project, SensorContext context) {
        sourceMap = loadProjectTree(project.getFileSystem(), project.getFileSystem().getBasedir().getPath(), context);
        analyse(project.getFileSystem(), context);
        onFinished();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void parseDirs(SensorContext context, List<File> files, List<File> sourceDirs, boolean unitTest, Charset sourcesEncoding) {
        for (File file : files) {
            Resource resource = sourceMap.get(file.getName());
            if (resource != null) {
                try {
                    String source = FileUtils.readFileToString(file, sourcesEncoding.name());
                    context.saveSource(resource, source);
                } catch (IOException e) {
                    throw new SonarException("Unable to read and import the source file : '" + file.getAbsolutePath() + "' with the charset : '"
                            + sourcesEncoding.name() + "'.", e);
                }
            }
        }
    }

    protected Resource createResource(File file, List<File> sourceDirs, boolean unitTest, String project, String directory) {
        return file != null ? AdaFile.fromIOFile(file, sourceDirs, project, directory) : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void onFinished() {
        lock.lock();
    }

    /**
     * Retrieve Ada project logical tree
     */
    protected Map<String, Resource> loadProjectTree(ProjectFileSystem fileSystem, String baseDirPath, SensorContext context) {
        Map<String, Resource> srcMap = new HashMap<String, Resource>();
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
                            Resource resource = createResource(new File(srcDir.toString(), iterator.next()), fileSystem.getSourceDirs(), false, project.toString(), srcDir.toString());
                            srcMap.put(resource.getName(), resource);
                        }
                    }

                }

            } catch (org.json.simple.parser.ParseException ex) {
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
