/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada;

import org.junit.Test;

public class Z999Test {

    @Test(expected = AssertionError.class)
    public void testz999() {
        assert (1 == 2);
    }
}
