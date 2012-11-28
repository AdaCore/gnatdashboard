/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import com.google.common.collect.Multiset;
import com.google.common.collect.TreeMultiset;
import java.util.Map;
import org.sonar.api.batch.Decorator;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependedUpon;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.Metric;
import org.sonar.api.measures.MeasuresFilters;
import org.sonar.api.measures.RuleMeasure;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.RuleUtils;
import org.sonar.api.utils.KeyValueFormat;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.utils.AdaUtils;

public class GnatCheckWeightedViolationsDecorator implements Decorator {

    private Map<RulePriority, Integer> weights;

    @DependsUpon
    public Metric dependsUponGnatCheckViolations() {
        return GnatCheckMetrics.GNATCHECK_VIOLATIONS;
    }

    @DependedUpon
    public Metric generatesGnatCheckWeightedViolations() {
        return GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS;
    }

    public GnatCheckWeightedViolationsDecorator() {
    }

    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguageKey().equals(Ada.KEY);
    }

    private void loadWeights(DecoratorContext context) {
        if (weights == null && context != null) {
            weights = RuleUtils.getPriorityWeights(context.getProject().getConfiguration());
        }
    }

    public void decorate(Resource resource, DecoratorContext context) {
        loadWeights(context);

        double debt = 0.0;
        Multiset<RulePriority> violationsByPriority = TreeMultiset.create();
        for (RuleMeasure violations : context.getMeasures(MeasuresFilters.rules(GnatCheckMetrics.GNATCHECK_VIOLATIONS))) {
            if (MeasureUtils.hasValue(violations)) {
                violationsByPriority.add(violations.getRulePriority(), violations.getValue().intValue());
                double add = (int) weights.get(violations.getRulePriority()) * violations.getValue();
                debt += add;
            }
        }
        Measure debtMeasure = new Measure(GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS, debt, KeyValueFormat.format(violationsByPriority));
        saveMeasure(context, debtMeasure);
    }

    private void saveMeasure(DecoratorContext context, Measure debtMeasure) {
        if (debtMeasure.getValue() > 0.0) {
            context.saveMeasure(debtMeasure);
        }
    }
}
