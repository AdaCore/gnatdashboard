/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import java.util.List;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import org.junit.Test;
import org.sonar.plugins.ada.TestUtils;


public class GnatMetricsTest {

    /**
     * Test of getMetrics method, of class GnatMetrics.
     */
    @Test
    public void testGetMetrics() {
        assertTrue(GnatMetrics.INSTANCE.getMetrics() != null);
        assertFalse(TestUtils.EMPTY_LIST_ERR_MSG,
                GnatMetrics.INSTANCE.getMetrics().isEmpty());
    }
}
