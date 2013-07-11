/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada;

import java.util.List;
import org.junit.Assert;
import org.junit.Test;
import org.sonar.colorizer.Tokenizer;

public class AdaColorizerTest {

    /**
     * Test of getTokenizers method, of class AdaColorizer.
     */
    @Test
    public void testGetTokenizers() {
        final AdaColorizer colorizer = new AdaColorizer();
        assertList(colorizer.getTokenizers());
        //Test the method a second time, first time the list is being initialized,
        //for other call the object attribut is returned.
        assertList(colorizer.getTokenizers());

    }

    public void assertList(List<Tokenizer> tokenizers){
        assert(tokenizers != null);
        Assert.assertEquals(3, tokenizers.size());
    }

}
