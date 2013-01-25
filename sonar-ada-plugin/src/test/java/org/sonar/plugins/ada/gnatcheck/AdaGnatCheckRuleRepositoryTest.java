/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import java.util.List;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import org.sonar.plugins.ada.Ada;

public class AdaGnatCheckRuleRepositoryTest {
    AdaGnatCheckRuleRepository repository;

    @Before
    public void setUp() {
        repository = new AdaGnatCheckRuleRepository();
    }

    @Test
    public void testFileName() {
        assertNotNull(repository.fileName());
        assertFalse(repository.fileName().isEmpty());
    }

    @Test
    public void testKey() {
        assertNotNull(repository.getKey());
        assertTrue("GNATcheck Rule Repository should be set",
                !repository.getKey().isEmpty());
    }

    @Test
    public void testLanguage() {
        assertEquals(repository.getLanguage(), Ada.INSTANCE.getKey());
    }

    @Test
    public void testCreatesRules() {
        List<org.sonar.api.rules.Rule> rules = repository.createRules();
        assertNotNull(rules);
        assertFalse("GNATcheck Rule Repository should not be empty",
                rules.isEmpty());
    }
}
