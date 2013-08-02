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
import org.sonar.plugins.ada.api.resource.AdaFile;


public class AdaCodePeerSensorTest {
    private SensorContext context;
    private Project project;
    private Configuration config;

    @Before
    public void setUp() {
        RuleFinder rulefinder = TestUtils.mockRuleFinder();
        AdaFile adaFileMock = mock(AdaFile.class);
        config = mock(Configuration.class);
        project = TestUtils.mockProject();
        context = mock(SensorContext.class);
        when(context.getResource((Resource)anyObject())).thenReturn(adaFileMock);
    }
}
