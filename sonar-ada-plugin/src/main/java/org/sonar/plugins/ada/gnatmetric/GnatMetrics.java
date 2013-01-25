/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatmetric;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.Metrics;
import org.sonar.api.measures.SumChildValuesFormula;
import org.sonar.api.utils.SonarException;

/**
 * Defines metrics specific to GnatMetric that cannot be bind to existent
 * Sonar's metric.
 */
public final class GnatMetrics implements Metrics {

    public static final GnatMetrics INSTANCE = new GnatMetrics();

    public static final Metric EOL_COMMENT = new Metric.Builder("eol_comments", "End of line comments", Metric.ValueType.INT)
                        .setDescription("Identified all end of line comments")
                        .setDirection(Metric.DIRECTION_NONE)
                        .setQualitative(Boolean.FALSE)
                        .setDomain(CoreMetrics.DOMAIN_DOCUMENTATION).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric BLANK_LINE = new Metric.Builder("blank_lines", "Blank lines", Metric.ValueType.INT)
                    .setDescription("Identified blank lines").setDirection(Metric.DIRECTION_WORST)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric ALL_SATEMENTS = new Metric.Builder("all_stmts", "All statements", Metric.ValueType.INT)
                    .setDescription("All statements in the file").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric ALL_DCLS = new Metric.Builder("all_dcls", "All declaration", Metric.ValueType.INT)
                    .setDescription("All declaration in the file").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric CONSTRUCT_NESTING = new Metric.Builder("construct_nesting", "Construct nesting", Metric.ValueType.INT)
                    .setDescription("Construct nesting").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric LSLOC = new Metric.Builder("lsloc", "Logical source line of code", Metric.ValueType.INT)
                    .setDescription("Logical source line of code").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

     public static final Metric STATEMENT_COMPLEXITY = new Metric.Builder("statement_complexity", "Statement complexity", Metric.ValueType.FLOAT)
                    .setDescription("Average of all statements complexity in the file").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

     public static final Metric SHORT_CIRCUIT_COMPLEXITY = new Metric.Builder("short_circuit_complexity", "Short circuit complexity", Metric.ValueType.FLOAT)
                    .setDescription("Short circuit complexity").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

     public static final Metric ESSENTIAL_COMPLEXITY = new Metric.Builder("essential_complexity", "Essential complexity", Metric.ValueType.FLOAT)
                    .setDescription("Essential complexity").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

    public static final Metric MAX_LOOP_NESTING = new Metric.Builder("max_loop_nesting", "Maximum loop nesting", Metric.ValueType.INT)
                    .setDescription("Maximum loop nesting").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric EXTRA_EXIT_POINTS = new Metric.Builder("extra_exit_points", "Extra exit points", Metric.ValueType.INT)
                    .setDescription("Extra exit points").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric UNIT_NESTING = new Metric.Builder("unit_nesting", "Unit nesting", Metric.ValueType.INT)
                    .setDescription("Unit nesting").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_COMPLEXITY).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric ALL_SUBPROGRAMS = new Metric.Builder("all_subprograms", "All subprograms", Metric.ValueType.INT)
                    .setDescription("All subprograms").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric ALL_TYPES = new Metric.Builder("all_types", "All types", Metric.ValueType.INT)
                    .setDescription("All types").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric PRIVATE_TYPES = new Metric.Builder("private_types", "Private types", Metric.ValueType.INT)
                    .setDescription("Private types").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric TAGGED_TYPES = new Metric.Builder("tagged_types", "Tagged types", Metric.ValueType.INT)
                    .setDescription("Tagged types").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric ABSTRACT_TYPES = new Metric.Builder("abstract_types", "Abstract types", Metric.ValueType.INT)
                    .setDescription("Abstract types").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric PUBLIC_TYPES = new Metric.Builder("public_types", "Public types", Metric.ValueType.INT)
                    .setDescription("Public types").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   public static final Metric PUBLIC_SUBPROGRAMS = new Metric.Builder("public_subprograms", "Public subprograms", Metric.ValueType.INT)
                    .setDescription("Public subprograms").setDirection(Metric.DIRECTION_NONE)
                    .setQualitative(Boolean.FALSE)
                    .setDomain(CoreMetrics.DOMAIN_SIZE).create().setFormula(new SumChildValuesFormula(false));

   private  static List<Metric> metrics = new ArrayList<Metric>();

    /**
     * Is defined in the Metrics interface and is used by Sonar to retrieve the
     * list of new metrics.
     */
    public List<Metric> getMetrics() {
    if (metrics.isEmpty()) {
      for (Field field : GnatMetrics.class.getFields()) {
        if (Metric.class.isAssignableFrom(field.getType())) {
          try {
              metrics.add((Metric) field.get(null));
          } catch (IllegalAccessException e) {
            throw new SonarException("can not load metrics from " + GnatMetrics.class.getSimpleName(), e);
          }
        }
      }
    }
    return metrics;
  }
}
