/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.ui.client;

import org.sonar.plugins.ada.ui.gwt.client.GwtGnatMetrics;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.sonar.plugins.ada.gnatmetric.GnatMetrics;

public class GwtGnatMetricsTest {

    @Test
    public void testNumberOfDeclaredMetrics() {
      assertEquals(GwtGnatMetrics.class.getDeclaredFields().length, GnatMetrics.INSTANCE.getMetrics().size());
    }
}
