/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.ui.gwt.client;

/**
 * Metric's key specific to GnatMetric. Accessible by GWT module for GnatMetric
 */
public interface GwtGnatMetrics {

    // ---------------
    //     SIZE
    //----------------
    String LSLOC = "lsloc";
    String BLANK_LINE = "blank_lines";

    //----------------
    // DOCUMENTATION
    //----------------
    String EOL_COMMENTS = "eol_comments";

    //----------------
    //   COMPLEXITY
    //----------------
    String STATEMENT_COMPLEXITY = "statement_complexity";
    String EXPRESSION_COMPLEXITY = "expression_complexity";
    String ESSENTIAL_COMPLEXITY = "essential_complexity";
    String MAX_LOOP_NESTING = "max_loop_nesting";
}
