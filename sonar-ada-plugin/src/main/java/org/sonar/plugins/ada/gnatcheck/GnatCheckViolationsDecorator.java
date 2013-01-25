/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.sonar.api.batch.Decorator;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.RuleMeasure;
import org.sonar.api.measures.MeasuresFilters;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.utils.AdaUtils;

public class GnatCheckViolationsDecorator implements Decorator {

    private Integer total = 0;
    private Multiset<Rule> rules = HashMultiset.create();
    private Multiset<RulePriority> severities = HashMultiset.create();
    private Map<Rule, RulePriority> ruleToSeverity = Maps.newHashMap();

    public void decorate(Resource resource, DecoratorContext context) {
        if (shouldDecorateResource(resource)) {
            resetCounters();
            countViolations(context);
            saveTotalViolations(context);
            saveViolationsBySeverity(context);
            saveViolationsByRule(context);
        }
    }

    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguage().getKey().equals(Ada.KEY);
    }

    private boolean shouldDecorateResource(Resource resource) {
        return resource.getLanguage().getKey().equals(Ada.KEY);
    }

    private void resetCounters() {
        rules.clear();
        severities.clear();
        ruleToSeverity.clear();
        total = 0;
    }

    private void saveViolationsBySeverity(DecoratorContext context) {
        for (RulePriority severity : RulePriority.values()) {
            Metric metric = AdaUtils.getMetricByRuleRepoSeverityAndCategory(AdaGnatCheckRuleRepository.KEY, "GnatCheckMetrics", severity.toString(), null);
            if (metric != null && context.getMeasure(metric) == null) {
                Collection<Measure> children = context.getChildrenMeasures(MeasuresFilters.metric(metric));
                Double sum = MeasureUtils.sum(true, children) + severities.count(severity);
                context.saveMeasure(metric, sum);
            }
        }
    }

    private void saveViolationsByRule(DecoratorContext context) {
        Collection<Measure> children = context.getChildrenMeasures(MeasuresFilters.rules(GnatCheckMetrics.GNATCHECK_VIOLATIONS));
        for (Measure childMeasure : children) {
            RuleMeasure childRuleMeasure = (RuleMeasure) childMeasure;
            Rule rule = childRuleMeasure.getRule();
            if (rule != null && MeasureUtils.hasValue(childRuleMeasure)) {
                rules.add(rule, childRuleMeasure.getValue().intValue());
                ruleToSeverity.put(childRuleMeasure.getRule(), childRuleMeasure.getRulePriority());
            }
        }
        for (Multiset.Entry<Rule> entry : rules.entrySet()) {
            Rule rule = entry.getElement();
            RuleMeasure measure = RuleMeasure.createForRule(GnatCheckMetrics.GNATCHECK_VIOLATIONS, rule, (double) entry.getCount());
            measure.setRulePriority(ruleToSeverity.get(rule));
            context.saveMeasure(measure);
        }
    }

    private void saveTotalViolations(DecoratorContext context) {
        // Finds violations measure for the resource
        if (context.getMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS) == null) {
            Collection<Measure> childrenViolations = context.getChildrenMeasures(GnatCheckMetrics.GNATCHECK_VIOLATIONS);
            //For source files sum of children = 0, for directories total = 0
            Double sum = MeasureUtils.sum(true, childrenViolations) + total;
            //Add the measure for the current resource
            context.saveMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS, sum);
        }
    }

    private void countViolations(DecoratorContext context) {
        List<Violation> violations = context.getViolations();

        for (Violation violation : violations) {
            Rule rule = violation.getRule();

            if (rule.getRepositoryKey().equals(AdaGnatCheckRuleRepository.KEY)) {
                total++;
                rules.add(violation.getRule());
                severities.add(rule.getSeverity());
                ruleToSeverity.put(rule, violation.getSeverity());
            }
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
