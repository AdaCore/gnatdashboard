/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.any;


import org.sonar.api.resources.Java;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.TestUtils;
import org.sonar.plugins.ada.resources.AdaFile;

public class GnatCheckViolationsDensityDecoratorTest {

    private GnatCheckViolationsDensityDecorator decorator;
    private AdaFile resourceMock;
    private File javaResourceMock;
    private DecoratorContext context;
    private DecoratorContext noDensityContext;
    private DecoratorContext noWeightContext;
    private Project adaProject;

    @Before
    public void setUp() throws Exception {
        adaProject = TestUtils.mockProject();
        decorator = new GnatCheckViolationsDensityDecorator();
        resourceMock = new AdaFile("mock", "mock", "mock");
        context = mock(DecoratorContext.class);
    }

    /**
     * Test of decorate method, of class GnatCheckViolationsDensityDecorator.
     */
    @Test
    public void testDecorateAdaFile() {
        // Configure mock context - to return measure for decorator's required
        // metrics
        when(context.getMeasure(CoreMetrics.NCLOC))
                .thenReturn(new Measure(CoreMetrics.NCLOC, 20d));
        when(context.getMeasure(GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS))
                .thenReturn(new Measure(GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS, 5d));
        when(context.getMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS_DENSITY))
                .thenReturn(null);
        when(context.getProject()).thenReturn(adaProject);

        decorator.decorate(resourceMock, context);
        verify(context, times(1)).saveMeasure(any(Metric.class),
                any(Double.class));
    }

    @Test
    public void testShouldNotDecorate() {
        // Mock Java project
        Project javaProject = new Project("JavaProject");
        javaProject.setLanguage(Java.INSTANCE);

        // Mock Java file
        javaResourceMock = new File("JavaFile");
        javaResourceMock.setLanguage(Java.INSTANCE);

        // Configure mock context: return Java project
        when(context.getProject()).thenReturn(javaProject);

        // Execution
        decorator.decorate(javaResourceMock, context);
        verify(context, times(0)).saveMeasure(any(Metric.class),
                any(Double.class));

    }

    /**
     * Test of toString method, of class GnatCheckViolationsDensityDecorator.
     */
    @Test
    public void testToString() {
        assertTrue(decorator.toString() != null);
        assertFalse(decorator.toString().isEmpty());
    }

    @Test
    public void dependsUponGnatCheckWeightedViolationsAndLsloc() {
        assertTrue(decorator.dependsUponGnatCheckWeightedViolationsAndLsloc() != null);
        assertFalse(decorator.dependsUponGnatCheckWeightedViolationsAndLsloc().isEmpty());
    }

    @Test
    public void generatesGnatCheckViolationsDensity() {
        assertTrue(decorator.generatesGnatCheckViolationsDensity() != null);
    }

    @Test
    public void shouldExecuteOnProject() {
        assertTrue(decorator.shouldExecuteOnProject(adaProject));
    }
}
