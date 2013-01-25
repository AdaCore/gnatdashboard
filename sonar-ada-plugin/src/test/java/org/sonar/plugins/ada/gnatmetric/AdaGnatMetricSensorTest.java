/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import org.apache.commons.configuration.Configuration;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.TestUtils;
import org.sonar.plugins.ada.resources.AdaFile;


public class AdaGnatMetricSensorTest {
    private AdaGnatMetricSensor sensor;
    private SensorContext context;
    private Configuration conf;
    private Project project;

    @Before
    public void setUp() {
        project = TestUtils.mockProject();
        conf = mock(Configuration.class);
        sensor = new AdaGnatMetricSensor(conf);
        context = mock(SensorContext.class);
        AdaFile adaFileMock = mock(AdaFile.class);
        when(context.getResource((Resource)anyObject())).thenReturn(adaFileMock);

    }

    @Test
    public void testSaveCorrectNumberMeasures(){
        sensor.analyse(project, context);
        verify(context, times(24)).saveMeasure(any(AdaFile.class),
                                               any(Metric.class),
                                               any(Double.class));
    }


    @Test
    public void testDontSaveMeasureWhenWrongReport(){
        //Set configure mock: return a wrong report.
        Configuration config = mock(Configuration.class);
        when(conf.getString(AdaGnatMetricSensor.REPORT_PATH_KEY))
                .thenReturn("reports/wrong-gnatmetric-report.xml");

        // Execution
        sensor.analyse(project, context);
        verify(context, times(0)).saveMeasure(any(AdaFile.class),
                                              any(Metric.class),
                                              any(Double.class));
    }

    /**
     * Test of reportPathKey method, of class AdaGnatMetricSensor.
     */
    @Test
    public void testReportPathKey() {
        assertTrue(sensor.reportPathKey() != null);
        assertFalse(sensor.reportPathKey().isEmpty());
    }
}
