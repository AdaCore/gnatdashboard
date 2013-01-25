/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CodePeerRuleCategoryTest {

    /**
     * Test of values method, of class CodePeerRuleCategory.
     */
    @Test
    public void testValues() {
        assertTrue(CodePeerRuleCategory.values().length == 4);
    }

    /**
     * Test of valueOf method, of class CodePeerRuleCategory.
     */
    @Test
    public void testValueOf() {
        assertTrue(CodePeerRuleCategory.valueOf("CHECK") != null);
        assertTrue(CodePeerRuleCategory.valueOf("RACE_CONDITION") != null);
        assertTrue(CodePeerRuleCategory.valueOf("WARNING") != null);
    }
}
