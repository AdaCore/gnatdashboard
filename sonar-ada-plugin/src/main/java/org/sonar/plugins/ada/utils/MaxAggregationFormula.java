/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.utils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.sonar.api.measures.Formula;
import org.sonar.api.measures.FormulaContext;
import org.sonar.api.measures.FormulaData;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.Metric;

public class MaxAggregationFormula implements Formula {

    public List<Metric> dependsUponMetrics() {
        return Collections.emptyList();
    }

    public Measure calculate(FormulaData data, FormulaContext context) {
        Integer max = 0;
        Collection<Measure> measures = data.getChildrenMeasures(context.getTargetMetric());
        for (Measure measure : measures) {
            if (MeasureUtils.hasValue(measure) && measure.getValue() > max) {
                max = measure.getIntValue();
            }
        }
        return new Measure(context.getTargetMetric(), max.doubleValue());
    }
}
