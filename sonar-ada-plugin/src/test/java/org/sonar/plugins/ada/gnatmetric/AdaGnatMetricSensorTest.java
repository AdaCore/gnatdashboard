/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.plugins.ada.api.resource.AdaFile;


public class AdaGnatMetricSensorTest {
    private AdaGnatMetricSensor sensor;
    private SensorContext context;
    private Configuration conf;
    private Project project;

    @Before
    public void setUp() {
    }

    @Test
    public void testSaveCorrectNumberMeasures(){
        sensor.analyse(project, context);
        verify(context, times(12)).saveMeasure(any(AdaFile.class),
                                               any(Metric.class),
                                               any(Double.class));
    }

    @Test
    public void testDontSaveMeasureWhenWrongReport(){
        Configuration config = mock(Configuration.class);
        when(conf.getString(""))
                .thenReturn("reports/wrong-gnatmetric-report.xml");

        // Execution
        sensor.analyse(project, context);
        verify(context, times(0)).saveMeasure(any(AdaFile.class),
                                              any(Metric.class),
                                              any(Double.class));
    }
}
