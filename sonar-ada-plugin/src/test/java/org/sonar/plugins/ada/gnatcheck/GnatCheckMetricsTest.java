/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import org.sonar.plugins.ada.TestUtils;

public class GnatCheckMetricsTest {

    /**
     * Test of getMetrics method, of class GnatCheckMetrics.
     */
    @Test
    public void testGetMetrics() {
      assertNotNull(GnatCheckMetrics.INSTANCE.getMetrics());
      assertFalse(TestUtils.EMPTY_LIST_ERR_MSG,
              GnatCheckMetrics.INSTANCE.getMetrics().isEmpty());
    }

}
