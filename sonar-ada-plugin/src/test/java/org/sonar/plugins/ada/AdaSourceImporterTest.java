/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import org.apache.commons.configuration.Configuration;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.sonar.api.batch.ResourceCreationLock;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.resources.AdaFile;


public class AdaSourceImporterTest {
    private AdaSourceImporter sourceImporter;
    private Project project;
    private SensorContext context;

    @Before
    public void setUp() {
        Configuration conf = mock(Configuration.class);
        ResourceCreationLock lock = mock(ResourceCreationLock.class);
        when(conf.getString(AdaSourceImporter.PROJECT_TREE_FILE_PATH_KEY)).
                thenReturn("project_tree.json");
        sourceImporter = new AdaSourceImporter(lock, conf);
    }

    /**
     * Test of analyse method, of class AdaSourceImporter.
     */
    @Test
    public void testAnalyse() {
        TestUtils.createProjectTree();
        project = TestUtils.mockProject();
        context = mock(SensorContext.class);

        sourceImporter.analyse(project, context);
        verify(context, times(4)).saveSource(any(AdaFile.class),
                                             any(String.class));
    }

    /**
     * Test of toString method, of class AdaSourceImporter.
     */
    @Test
    public void testToString() {
       assertNotNull(sourceImporter.toString());
       assertFalse(TestUtils.TO_STRING_ERR_MSG,sourceImporter.toString().isEmpty());
    }
}
