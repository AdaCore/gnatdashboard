/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.any;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;

import org.apache.commons.configuration.Configuration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.TestUtils;
import org.sonar.plugins.ada.resources.AdaFile;


public class AdaCodePeerSensorTest {
    private AdaCodePeerSensor sensor;
    private SensorContext context;
    private Project project;
    private Configuration config;

    @Before
    public void setUp() {
        RuleFinder rulefinder = TestUtils.mockRuleFinder();
        AdaFile adaFileMock = mock(AdaFile.class);
        config = mock(Configuration.class);
        project = TestUtils.mockProject();
        sensor = new AdaCodePeerSensor(rulefinder, config);
        context = mock(SensorContext.class);
        when(context.getResource((Resource)anyObject())).thenReturn(adaFileMock);
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
        when(config.getString(AdaCodePeerSensor.REPORT_PATH_KEY))
                .thenReturn("reports/wrong-codepeer-report.json");
        sensor.analyse(project, context);
        verify(context, times(0)).saveViolation(any(Violation.class));
    }
}
