/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class AdaTest {

    @Test
    public void testGetFileSuffixes() {
        String [] suffixes = Ada.INSTANCE.getFileSuffixes();
        assertTrue("Ada file has no specific suffixes.",suffixes.length == 0);
    }
}
