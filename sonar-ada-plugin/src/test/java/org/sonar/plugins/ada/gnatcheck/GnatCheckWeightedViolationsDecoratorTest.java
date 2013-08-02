/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.configuration.Configuration;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.sonar.api.CoreProperties;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasuresFilter;
import org.sonar.api.measures.RuleMeasure;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.RulePriority;
import org.sonar.plugins.ada.TestUtils;
import org.sonar.plugins.ada.api.resource.AdaFile;

public class GnatCheckWeightedViolationsDecoratorTest {

    private GnatCheckWeightedViolationsDecorator decorator;
    private DecoratorContext context;
    private AdaFile resourceMock;
    private Project project;

    @Before
    public void setUp() {
        decorator = new GnatCheckWeightedViolationsDecorator();
        resourceMock = new AdaFile("mock", "mock", "mock");
        context = mock(DecoratorContext.class);
        project = TestUtils.mockProject();

        // Configure project to return configuration with default rule weight
        Configuration conf = mock(Configuration.class);
        when(project.getConfiguration()).thenReturn(conf);
        when(conf.getString((String) anyObject())).thenReturn(CoreProperties.CORE_RULE_WEIGHTS_DEFAULT_VALUE);
    }

    /**
     * Test of decorate method, of class GnatCheckWeightedViolationsDecorator.
     */
    @Test
    public void testDecorate() {
        // Configure context mock:
        // return non-empty list of measures - return an Ada project
        List<RuleMeasure> measures = createRuleMeasure();
        when(context.getMeasures((MeasuresFilter) anyObject())).thenReturn(measures);
        when(context.getProject()).thenReturn(project);

        decorator.decorate(resourceMock, context);
        verify(context, times(1)).saveMeasure(any(Measure.class));
    }

    @Test
    public void testDependsUponGnatCheckViolations() {
        assertNotNull(decorator.dependsUponGnatCheckViolations());
    }

    @Test
    public void testGeneratesGnatCheckWeightedViolations() {
        assertNotNull(decorator.generatesGnatCheckWeightedViolations());
    }

    @Test
    public void testShouldExecuteOnProject() {
        assertTrue(decorator.shouldExecuteOnProject(TestUtils.mockProject()));
    }

    public List<RuleMeasure> createRuleMeasure() {
        List<RuleMeasure> measures = new ArrayList<RuleMeasure>();

        //Measure with all attributes OK
        RuleMeasure rm = RuleMeasure.createForRule(GnatCheckMetrics.GNATCHECK_VIOLATIONS,
                TestUtils.mockRule(AdaGnatCheckRuleRepository.KEY,
                RulePriority.MAJOR), 3d);

        //Measure with null value
        RuleMeasure rmNoValue = RuleMeasure.createForRule(GnatCheckMetrics.GNATCHECK_VIOLATIONS,
                TestUtils.mockRule(AdaGnatCheckRuleRepository.KEY,
                RulePriority.MAJOR), null);
        rm.setSeverity(RulePriority.MAJOR);
        rmNoValue.setSeverity(RulePriority.MAJOR);

        //Measure with value = 0
        RuleMeasure rmZero = RuleMeasure.createForRule(GnatCheckMetrics.GNATCHECK_VIOLATIONS,
                TestUtils.mockRule(AdaGnatCheckRuleRepository.KEY,
                RulePriority.MAJOR), 0.0);
        rmZero.setSeverity(RulePriority.MAJOR);

        // Add all measures to list
        measures.add(rm);
        measures.add(rmNoValue);
        measures.add(rmZero);

        return measures;
    }
}
