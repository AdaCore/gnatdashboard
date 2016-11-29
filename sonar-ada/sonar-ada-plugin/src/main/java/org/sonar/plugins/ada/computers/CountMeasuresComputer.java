/*
 * GNATdashboard
 * Copyright (C) 2016, AdaCore
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package org.sonar.plugins.ada.computers;

import org.sonar.api.ce.measure.Component;
import org.sonar.api.ce.measure.Issue;
import org.sonar.api.ce.measure.Measure;
import org.sonar.api.ce.measure.MeasureComputer;
import org.sonar.plugins.ada.metrics.CountMetrics;
import org.sonar.plugins.ada.rules.CodePeerRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.GNATcheckRulesDefinitionXmlLoader;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Measures computer that aggregates totals.
 */
public class CountMeasuresComputer implements MeasureComputer {
  private static final String CODEPEER = CodePeerRulesDefinitionXmlLoader.REPOSITORY_KEY;
  private static final String GNATCHECK = GNATcheckRulesDefinitionXmlLoader.REPOSITORY_KEY;

  @Override
  public MeasureComputerDefinition define(final MeasureComputerDefinitionContext defContext) {
    return defContext.newDefinitionBuilder()
        .setOutputMetrics(CountMetrics.CODEPEER.key(), CountMetrics.GNATCHECK.key())
        .build();
  }

  @Override
  public void compute(final MeasureComputerContext context) {
    if (context.getComponent().getType().equals(Component.Type.FILE)) {
      // On files, count the number of such issues per tool.
      context.addMeasure(CountMetrics.CODEPEER.key(), context.getIssues().stream()
          .filter(issue -> CODEPEER.equals(issue.ruleKey().repository()))
          .collect(Collectors.<Issue>toList()).size());
      context.addMeasure(CountMetrics.GNATCHECK.key(), context.getIssues().stream()
          .filter(issue -> GNATCHECK.equals(issue.ruleKey().repository()))
          .collect(Collectors.<Issue>toList()).size());
    } else {
      // Otherwise aggregates totals.
      context.addMeasure(CountMetrics.CODEPEER.key(), StreamSupport.stream(
          context.getChildrenMeasures(CountMetrics.CODEPEER.key()).spliterator(), false
      ).mapToInt(Measure::getIntValue).sum());
      context.addMeasure(CountMetrics.GNATCHECK.key(), StreamSupport.stream(
          context.getChildrenMeasures(CountMetrics.GNATCHECK.key()).spliterator(), false
      ).mapToInt(Measure::getIntValue).sum());
    }
  }
}
