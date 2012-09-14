/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.sonar.colorizer.Tokenizer;

public class AdaColorizerTest {

    /**
     * Test of getTokenizers method, of class AdaColorizer.
     */
    @Test
    public void testGetTokenizers() {
        List<Tokenizer> tokenizer = (new AdaColorizer()).getTokenizers();
        assert(tokenizer != null);
        assertEquals(3, tokenizer.size());
    }
}
