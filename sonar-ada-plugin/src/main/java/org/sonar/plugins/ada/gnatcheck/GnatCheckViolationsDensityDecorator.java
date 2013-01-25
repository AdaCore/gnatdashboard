/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.gnatcheck;

import java.util.Arrays;
import java.util.List;
import org.sonar.api.batch.Decorator;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependedUpon;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.Measure;
import org.sonar.api.measures.MeasureUtils;
import org.sonar.api.measures.Metric;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.ada.Ada;

public class GnatCheckViolationsDensityDecorator implements Decorator {

    public GnatCheckViolationsDensityDecorator() {
    }

    public boolean shouldExecuteOnProject(Project project) {
        return project.getLanguage().getKey().equals(Ada.KEY);
    }

    @DependsUpon
    public List<Metric> dependsUponGnatCheckWeightedViolationsAndLsloc() {
        return Arrays.asList(GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS, CoreMetrics.NCLOC);
    }

    @DependedUpon
    public Metric generatesGnatCheckViolationsDensity() {
        return GnatCheckMetrics.GNATCHECK_VIOLATIONS_DENSITY;
    }

    public void decorate(Resource resource, DecoratorContext context) {
        if (shouldDecorateResource(context)) {
            decorateDensity(context);
        }
    }

  protected boolean shouldDecorateResource(DecoratorContext context) {
    return context.getProject().getLanguage().getKey().equals(Ada.KEY) &&
            context.getMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS_DENSITY) == null;
  }

  private void decorateDensity(DecoratorContext context) {
    Measure ncloc = context.getMeasure(CoreMetrics.NCLOC);
    if (MeasureUtils.hasValue(ncloc) && ncloc.getValue() > 0.0) {
      saveDensity(context, ncloc.getValue().intValue());
    }
  }

  private void saveDensity(DecoratorContext context, int ncloc) {
    Measure debt = context.getMeasure(GnatCheckMetrics.GNATCHECK_WEIGHTED_VIOLATIONS);
    Integer debtValue = 0;
    if (MeasureUtils.hasValue(debt)) {
      debtValue = debt.getValue().intValue();
    }
    double density = calculate(debtValue, ncloc);
    context.saveMeasure(GnatCheckMetrics.GNATCHECK_VIOLATIONS_DENSITY, density);
  }

  protected static double calculate(int debt, int ncloc) {
    double rci = (1.0 - ((double) debt / (double) ncloc)) * 100.0;
    rci = Math.max(rci, 0.0);
    return rci;
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }
}
