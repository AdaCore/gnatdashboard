/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import org.sonar.plugins.ada.TestUtils;

public class CodePeerMetricsTest {

    /**
     * Test of getMetrics method, of class CodePeerMetrics.
     */
    @Test
    public void testGetMetrics() {
      assertNotNull(CodePeerMetrics.INSTANCE.getMetrics());
      assertFalse(TestUtils.EMPTY_LIST_ERR_MSG,
              CodePeerMetrics.INSTANCE.getMetrics().isEmpty());
    }

}
