/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import org.apache.commons.configuration.Configuration;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.anyObject;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.any;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.TestUtils;
import org.sonar.plugins.ada.resources.AdaFile;

public class AdaGnatCheckSensorTest {
    private AdaGnatCheckSensor sensor;
    private SensorContext context;
    private Configuration config;
    private Project project;

    @Before
    public void setUp() {
        RuleFinder rulefinder = TestUtils.mockRuleFinder();
        AdaFile adaFileMock = mock(AdaFile.class);
        config = mock(Configuration.class);
        project = TestUtils.mockProject();
        sensor = new AdaGnatCheckSensor(rulefinder, config);
        context = mock(SensorContext.class);
        when(context.getResource((Resource) anyObject())).thenReturn(adaFileMock);
    }

    /**
     * Test of reportPathKey method, of class AdaGnatCheckSensor.
     */
    @Test
    public void testReportPathKey() {
        assertNotNull(sensor.reportPathKey());
        assertFalse(sensor.reportPathKey().isEmpty());
    }

    @Test
    public void testReportCorrectNumberViolations() {
        sensor.analyse(project, context);
        verify(context, times(3)).saveViolation(any(Violation.class));
    }

    @Test
    public void testWrongReport() {
        when(config.getString(AdaGnatCheckSensor.REPORT_PATH_KEY))
                .thenReturn("reports/wrong-gnatcheck-report.xml");
        sensor.analyse(project, context);
        verify(context, times(0)).saveViolation(any(Violation.class));
    }
}
