/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.ui.gwt.client;

/**
 * Metric's key specific to GnatMetric. Accessible by GWT module for GnatMetric
 */
public interface GwtGnatMetrics {

    String BLANK_LINE = "blank_lines";
    String EOL_COMMENTS = "eol_comments";
    String ALL_SATEMENTS = "all_stmts";
    String ALL_DCLS = "all_dcls";
    String CONSTRUCT_NESTING = "construct_nesting";
    String LSLOC = "lsloc";
    String STATEMENT_COMPLEXITY = "statement_complexity";
    String SHORT_CIRCUIT_COMPLEXITY = "short_circuit_complexity";
    String ESSENTIAL_COMPLEXITY = "essential_complexity";
    String MAX_LOOP_NESTING = "max_loop_nesting";
    String EXTRA_EXIT_POINTS = "extra_exit_points";
    String UNIT_NESTING = "unit_nesting";
    String ALL_SUBPROGRAMS = "all_subprograms";
    String ALL_TYPES = "all_types";
    String TAGGED_TYPES = "tagged_types";
    String ABSTRACT_TYPES = "abstract_types";
    String PRIVATE_TYPES = "private_types";
    String PUBLIC_TYPES = "public_types";
    String PUBLIC_SUBPROGRAMS = "public_subprograms";
}
