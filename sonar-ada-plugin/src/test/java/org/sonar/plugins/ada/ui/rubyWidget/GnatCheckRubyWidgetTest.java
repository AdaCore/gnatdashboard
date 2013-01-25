/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.ui.rubyWidget;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import org.junit.Before;
import org.junit.Test;

public class GnatCheckRubyWidgetTest {

    private GnatCheckViolationsRubyWidget widget;

    @Before
    public void setUp() {
        widget = new GnatCheckViolationsRubyWidget();
    }

    /**
     * Test of getId method, of class CodePeerViolationsRubyWidget.
     */
    @Test
    public void testGetId() {
        assertTrue(widget.getId() != null);
        assertFalse(widget.getId().isEmpty());
    }

    /**
     * Test of getTitle method, of class CodePeerViolationsRubyWidget.
     */
    @Test
    public void testGetTitle() {
        assertTrue(widget.getTemplatePath() != null);
        assertFalse(widget.getTemplatePath().isEmpty());
    }

    /**
     * Test of getTemplatePath method, of class CodePeerViolationsRubyWidget.
     */
    @Test
    public void testGetTemplatePath() {
        assertTrue(widget.getTitle() != null);
        assertFalse(widget.getTitle().isEmpty());
    }
}
