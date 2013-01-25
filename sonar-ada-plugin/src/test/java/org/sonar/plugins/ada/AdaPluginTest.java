/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class AdaPluginTest {
    private AdaPlugin plugin = new AdaPlugin();

    /**
     * Test of getExtensions method, of class AdaPlugin.
     */
    @Test
    public void testGetExtensions() {
        assertEquals(20, plugin.getExtensions().size());
    }

    @Test
    public void testToString(){
        String toString = plugin.toString();
        assertNotNull(toString);
        assertFalse(TestUtils.TO_STRING_ERR_MSG, toString.isEmpty());
    }
}
