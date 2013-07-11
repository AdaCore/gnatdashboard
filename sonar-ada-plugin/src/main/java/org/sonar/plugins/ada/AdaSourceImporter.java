/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.ResourceCreationLock;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.utils.SonarException;
import org.sonar.plugins.ada.persistence.AdaDao;
import org.sonar.plugins.ada.persistence.JDBCUtils;
import org.sonar.plugins.ada.resources.AdaFile;

/**
 * Extension of AbstractSourceImporter for Ada project import all project
 * sources.
 */
@Phase(name = Phase.Name.PRE)
public class AdaSourceImporter extends AbstractSourceImporter {

    // <editor-fold desc="Class's attibutes" defaultstate="collapsed">

    /**
     * Contains the mapping of all project's resources: key: resource basename
     * value: AdaFile Sonar representation of the resource
     */
    private static Map<String, AdaFile> sourceMap = new HashMap<String, AdaFile> ();

    //TO BE REMOVED !!
    public static Map<String, AdaFile> getSourceMap() {
        return sourceMap;
    }
    private Logger logger = Logger.getLogger(AdaSourceImporter.class);
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
            saveResources(context, project.getFileSystem().getSourceCharset());
        onFinished();
    }

    /**
     * Import resource content and save it in the context.
     *
     * @param context
     * @param source encoding
     */
    protected void saveResources(SensorContext context, Charset sourcesEncoding) {
        JDBCUtils.setDBurl(config.getString(JDBCUtils.QMT_DB_PATH));
        AdaDao dao = new AdaDao();

        for (AdaFile resource : dao.selectAllResources()) {
            File file = new File(resource.getLongName());
            logger.info("+++ File : " + file.getName());
            try {
                String source = FileUtils.readFileToString(file, sourcesEncoding.name());
                context.saveSource(resource, source);
                logger.info("File: " + resource.getName());
            } catch (FileNotFoundException ex){
               logger.info("File not found: " + file.getAbsolutePath());
            } catch (IOException e) {
                throw new SonarException("Unable to read and import the source file : '"
                        + file.getAbsolutePath() + "' with the charset : '"
                        + sourcesEncoding.name() + "'.", e);
            }
        }
    }

    protected Resource createResource(File file, boolean unitTest, String project, String directory) {
        return file != null ? AdaFile.fromIOFile(file, project, directory) : null;
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

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
