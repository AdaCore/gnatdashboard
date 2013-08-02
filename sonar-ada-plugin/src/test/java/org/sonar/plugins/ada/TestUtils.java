/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.RuleQuery;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.api.resource.AdaFile;

public class TestUtils {

    public static final Integer NB_VIOLATIONS = 3;
    public static final String TO_STRING_ERR_MSG = "toString method should not return an empty string";
    public static final String EMPTY_LIST_ERR_MSG = "Method should not return a empty list";

    public static File loadResource(String resourceName) {
        URL resource = TestUtils.class.getResource(resourceName);
        File resourceAsFile = null;
        try {
            resourceAsFile = new File(resource.toURI());
        } catch (URISyntaxException e) {
            System.out.println("Cannot load resource: " + resourceName);
        }
        return resourceAsFile;
    }

    public static Project mockProject() {
        File baseDir = loadResource("/org/sonar/plugins/ada/");
        List<File> sourceDirs = new ArrayList<File>();
        sourceDirs.add(new File(baseDir, "sources"));

        ProjectFileSystem fileSystem = mock(ProjectFileSystem.class);
        when(fileSystem.getBasedir()).thenReturn(baseDir);
        when(fileSystem.getSourceCharset()).thenReturn(Charset.defaultCharset());
        when(fileSystem.getSourceDirs()).thenReturn(sourceDirs);

        Project project = mock(Project.class);
        when(project.getFileSystem()).thenReturn(fileSystem);
        when(project.getLanguage()).thenReturn(Ada.INSTANCE);

        return project;
    }

    public static RuleFinder mockRuleFinder() {
        Rule ruleMock = Rule.create("", "", "");
        RuleFinder ruleFinder = mock(RuleFinder.class);
        when(ruleFinder.findByKey((String) anyObject(),
                (String) anyObject())).thenReturn(ruleMock);
        when(ruleFinder.find((RuleQuery) anyObject())).thenReturn(ruleMock);
        return ruleFinder;
    }

    public static Rule mockRule(String repository, RulePriority severity) {
        Rule ruleMock = Rule.create(repository, "", "");
        ruleMock.setSeverity(severity);
        return ruleMock;
    }

    public static List<Violation> mockViolations(String repository) {
        //Resource
        AdaFile resourceMock = new AdaFile("", "", "");

        //Rule
        Rule ruleMock = mockRule(repository, RulePriority.MAJOR);

        //Violations
        List<Violation> violations = new ArrayList<Violation>(NB_VIOLATIONS);
        for (int i = 0; i < NB_VIOLATIONS; ++i) {
            violations.add(Violation.create(ruleMock, resourceMock));
        }
        return violations;
    }

    public static void createProjectTree() {
        URL url = TestUtils.class.getResource("/org/sonar/plugins/ada/project_tree.json");
        if (url == null) {

            //common directory sources
            JSONArray commonSources = new JSONArray();
            commonSources.add("input.adb");
            commonSources.add("input.ads");
            File commonDir = loadResource("/org/sonar/plugins/ada/common");

            //struct directory source
            JSONArray structSources = new JSONArray();
            structSources.add("instructions.adb");
            structSources.add("instructions.ads");
            File structDir = loadResource("/org/sonar/plugins/ada/struct");

            JSONObject sourceDirs = new JSONObject();
            sourceDirs.put(structDir.getPath(), structSources);
            sourceDirs.put(commonDir.getPath(), commonSources);

            //Dirs
            JSONObject allDir = new JSONObject();
            allDir.put("Source_Dirs", sourceDirs);
            //not used, created only to reproduce a "real" json project tree file.
            allDir.put("Object_Dirs", "sdc/common");

            //Project
            JSONObject project = new JSONObject();
            project.put("Sdc", allDir);

            try {

                File projectTree = new File(loadResource("/org/sonar/plugins/ada/"), "project_tree.json");
                FileWriter file = new FileWriter(projectTree);
                file.write(project.toJSONString());
                file.flush();
                file.close();

            } catch (IOException e) {
                System.out.println("Unable to dump json project tree test file.");
            }
        }
    }
}
