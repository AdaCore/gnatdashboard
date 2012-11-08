/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.codepeer;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Maps;
import com.google.common.collect.Multiset;
import com.sun.tools.javac.util.Pair;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.sonar.api.batch.Decorator;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.MeasuresFilters;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RulePriority;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.utils.AdaUtils;

/**
 * Decorator for Codepeer metrics.
 *
 * Uses same algo than in org.sonar.plugins.core.sensors.ViolationsDecorator
 */
public class CodepeerDecorator implements Decorator {

    private Integer total = 0;
    private Map<Rule, RulePriority> ruleToSeverity = Maps.newHashMap();
    private Multiset<Rule> rules = HashMultiset.create();
    private Multiset<Pair> severitiesCategoies = HashMultiset.create();

    public void decorate(Resource resource, DecoratorContext context) {
        if (shouldDecorateResource(resource)) {
            resetCounters();
            countViolations(context);
            saveTotalViolations(context);
            saveViolationsBySeverityAndCategory(context);
        }
    }

    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguageKey().equals(Ada.KEY);
    }

    private boolean shouldDecorateResource(Resource resource) {
        return resource.getLanguage().getKey().equals(Ada.KEY);
    }

    private void resetCounters() {
        ruleToSeverity.clear();
        severitiesCategoies.clear();
        total = 0;
    }

    private void saveViolationsBySeverityAndCategory(DecoratorContext context) {
        for (CodepeerRuleCategory category : CodepeerRuleCategory.values()) {
            for (RulePriority severity : RulePriority.values()) {
                if (severity.equals(RulePriority.BLOCKER)) {
                    continue;
                }
                Metric metric = AdaUtils.getMetricBySeverityAndCategory(severity.toString(), category.toString());
                if (metric != null && context.getMeasure(metric) == null) {
                    Collection<Measure> children = context.getChildrenMeasures(MeasuresFilters.metric(metric));
                    Double sum = MeasureUtils.sum(true, children) + severitiesCategoies.count(new Pair(severity, category));
                    context.saveMeasure(new Measure(metric, sum));
                }
            }
        }
    }

    private void saveTotalViolations(DecoratorContext context) {
        // Finds violations measure for the resource
        if (context.getMeasure(CodepeerMetrics.CODEPEER_VIOLATIONS) == null) {
            Collection<Measure> childrenViolations = context.getChildrenMeasures(CodepeerMetrics.CODEPEER_VIOLATIONS);
            //For source files sum of children = 0, for directories total = 0
            Double sum = MeasureUtils.sum(true, childrenViolations) + total;
            //Add the measure for the current resource
            context.saveMeasure(new Measure(CodepeerMetrics.CODEPEER_VIOLATIONS, sum));
        }
    }

    private void countViolations(DecoratorContext context) {
        List<Violation> violations = context.getViolations();

        for (Violation violation : violations) {
            Rule rule = violation.getRule();

            if (rule.getRepositoryKey().equals(AdaCodepeerRuleRepository.KEY)) {
                String[] ruleKey = rule.getKey().split(":");
                //Skipping the violation if the rule key is unparsable,
                //expected pattern: "SEVERITY:CATEGORY:rule unique key
                if (!(ruleKey.length < 3)) {
                    try {
                        CodepeerRuleCategory category = CodepeerRuleCategory.valueOf(ruleKey[1]);
                        total++;
                        rules.add(violation.getRule());
                        severitiesCategoies.add(new Pair(rule.getSeverity(), category));
                    } catch (IllegalArgumentException ex) {
                        AdaUtils.LOG.warn("Skipping violation: {}, unknown category {}", violation.getMessage(), ruleKey[1]);
                    }
                } else {
                    AdaUtils.LOG.warn("Skipping rule's violation: {}, unable to parse Codepeer rule key: {}. Pattern should be: \"SEVERITY:CATEGORY:rule unique key\".", violation.getMessage(), rule.getKey());
                }
            }
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
