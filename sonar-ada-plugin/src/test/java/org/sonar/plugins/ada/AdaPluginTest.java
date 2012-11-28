/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class AdaPluginTest {

    /**
     * Test of getExtensions method, of class AdaPlugin.
     */
    @Test
    public void testGetExtensions() {
        AdaPlugin plugin = new AdaPlugin();
        assertEquals(19, plugin.getExtensions().size());
    }
}
