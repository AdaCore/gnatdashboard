/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import java.util.ArrayList;
import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.any;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasuresFilter;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.RuleMeasure;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Java;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.resources.AdaDirectory;
import org.sonar.plugins.ada.resources.AdaFile;
import org.sonar.plugins.ada.TestUtils;

public class GnatCheckViolationsDecoratorTest {

    private GnatCheckViolationsDecorator decorator;
    private DecoratorContext context;
    private AdaFile resourceMock;

    @Before
    public void setUp() {
        resourceMock = new AdaFile("", "", "");
        decorator = new GnatCheckViolationsDecorator();
        context = mock(DecoratorContext.class);
    }

    /**
     * Test that decorates an AdaFile resource.
     */
    @Test
    public void testDecorateAdaFile() {
        // Mock violations - add a non-GNATcheck violation
        List<Violation> violations = TestUtils.mockViolations(AdaGnatCheckRuleRepository.KEY);
        Violation nonGCViolation = Violation.create(TestUtils.mockRule("codepeer", RulePriority.INFO), resourceMock);
        violations.add(nonGCViolation);

        // Configure mock context:
        // violations for current resource - no children measure
        // - no measure saved yet for any metric.
        when(context.getViolations()).thenReturn(violations);
        when(context.getMeasure((Metric) anyObject())).thenReturn(null);
        when(context.getChildrenMeasures((MeasuresFilter) anyObject()))
                .thenReturn(new ArrayList<Measure>() {});

        // Execution
        decorator.decorate(resourceMock, context);
        verify(context, times(1)).saveMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS,
                TestUtils.NB_VIOLATIONS.doubleValue());
        verify(context, times(1)).saveMeasure(GnatCheckMetrics.GNATCHECK_MAJOR_VIOLATIONS,
                TestUtils.NB_VIOLATIONS.doubleValue());
    }

    /**
     * Test that decorates an AdaDirectory resource.
     */
    @Test
    public void testDecorateAdaDirectory() {
        // Configure mock context:
        // return children violations measures
        // - no measure saved for any metric
        List<Measure> measure = createChildrenMeasure();
        when(context.getMeasure((Metric) anyObject())).thenReturn(null);
        when(context.getChildrenMeasures((MeasuresFilter) anyObject())).thenReturn(measure);
        decorator.decorate(new AdaDirectory("mock", "mock"), context);
    }

    /**
     * Test that doesn't decorate an AdaFile resource where violations measure
     * have already been saved.
     */
    @Test
    public void testViolationsAlreadySaved() {
        // Configure mock context
        // violations measure already saved for the resource
        when(context.getMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS)).thenReturn(new Measure());

        // Execution
        decorator.decorate(resourceMock, context);
        verify(context, times(0)).saveMeasure(any(Measure.class));
    }

    /**
     * Test that doesn't decorate an non-AdaFile resource.
     */
    @Test
    public void testDontDecorateNonAdaFile() {
        // Mock a Java file
        File javaResourceMock = new File("JavaFile");
        javaResourceMock.setLanguage(Java.INSTANCE);

        // Execution
        decorator.decorate(javaResourceMock, context);
        verifyZeroInteractions(context);
    }

    /**
     * Test that decorates Ada project.
     */
    @Test
    public void testShouldExecuteOnProject() {
        assertTrue(decorator.shouldExecuteOnProject(TestUtils.mockProject()));
    }

    /**
     * Test that doesn't decorate non-Ada project.
     */
    @Test
    public void testShouldNotExecuteOnProject() {
        // Mock Java project
        Project javaProjectMock = new Project("Java");
        javaProjectMock.setLanguage(Java.INSTANCE);

        // Execution
        assertFalse(decorator.shouldExecuteOnProject(javaProjectMock));
    }

    /**
     * Test of toString method, of class GnatCheckViolationsDecorator.
     */
    @Test
    public void testToString() {
        assertNotNull(decorator.toString());
        assertFalse(decorator.toString().isEmpty());
    }

    public List<Measure> createChildrenMeasure() {
        List<Measure> measures = new ArrayList<Measure>();

        // Mock a rule
        Rule rulemock = TestUtils.mockRule("gnatcheck", RulePriority.MAJOR);

        //RuleMeasure noRuleAndValue = new RuleMeasure(GnatCheckMetrics.GNATCHECK_MAJOR_VIOLATIONS,
          //      null, RulePriority.MAJOR, null);
        RuleMeasure noRule = new RuleMeasure(GnatCheckMetrics.GNATCHECK_MAJOR_VIOLATIONS,
                null, RulePriority.MAJOR, 1);
        RuleMeasure noValue = new RuleMeasure(GnatCheckMetrics.GNATCHECK_MAJOR_VIOLATIONS,
                rulemock, RulePriority.MAJOR, null);
        RuleMeasure good = new RuleMeasure(GnatCheckMetrics.GNATCHECK_MAJOR_VIOLATIONS,
                rulemock, RulePriority.MAJOR, 2);

        RuleMeasure goodBis = mock(RuleMeasure.class);
        when(goodBis.getRule()).thenReturn(rulemock);
        when(goodBis.getValue()).thenReturn(2d);

        // Add all created measures to list
        measures.add(noRule);
        measures.add(noValue);
        measures.add(good);
        measures.add(goodBis);

        return measures;
    }
}
