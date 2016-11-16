/*
 * Sonar Ada Plugin (GNATdashboard)
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

package org.sonar.plugins.ada;

import lombok.AllArgsConstructor;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.Decorator;
import org.sonar.api.batch.DecoratorBarriers;
import org.sonar.api.batch.DecoratorContext;
import org.sonar.api.batch.DependsUpon;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.resources.ResourceUtils;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.ada.codepeer.CodePeerRulesDefinition;
import org.sonar.plugins.ada.gnat.GNATcheckRulesDefinition;
import org.sonar.plugins.ada.lang.Ada;

/**
 * Project decorator.
 *
 * Computes and saves the value of custom metrics such as the total CodePeer
 * issues raised or the total GNATcheck violations detected.
 */
@Slf4j
@AllArgsConstructor
@DependsUpon(DecoratorBarriers.ISSUES_TRACKED)
public class AdaCountIssuesDecorator implements Decorator {
  private final FileSystem fs;
  private final RulesProfile rules;
  private final ResourcePerspectives perspectives;

  @Override
  public boolean shouldExecuteOnProject(final Project project) {
    return fs.hasFiles(fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY)));
  }

  @Override
  public void decorate(
      final Resource resource, final DecoratorContext context) {
    if (!ResourceUtils.isFile(resource)) {
      // Only process files
      return;
    }

    @NonNull final Issuable issuable =
        perspectives.as(Issuable.class, resource);

    int codepeerCount = 0;
    int gnatcheckCount = 0;

    for (final Issue issue : issuable.issues()) {
      if (isCodePeerRule(issue.ruleKey())) {
        codepeerCount += 1;
      } else if (isGNATcheckRule(issue.ruleKey())) {
        gnatcheckCount += 1;
      }
    }

    context.saveMeasure(AdaMetrics.CODEPEER, (double) codepeerCount);
    log.debug("{}: saved {} CodePeer issues", resource, codepeerCount);

    context.saveMeasure(AdaMetrics.GNATCHECK, (double) gnatcheckCount);
    log.debug("{}: saved {} GNATcheck issues", resource, gnatcheckCount);
  }

  /**
   * Predicate indicating a CodePeer rule.
   *
   * @param rule The rule to analyse.
   * @return {@code True} if {@code rule} is a CodePeer rule, {@code false}
   * otherwise.
   */
  private static boolean isCodePeerRule(final RuleKey rule) {
    return CodePeerRulesDefinition.REPOSITORY_KEY.equals(rule.repository());
  }

  /**
   * Predicate indicating a GNATcheck rule.
   *
   * @param rule The rule to analyse.
   * @return {@code True} if {@code rule} is a GNATcheck rule, {@code false}
   * otherwise.
   */
  private static boolean isGNATcheckRule(final RuleKey rule) {
    return GNATcheckRulesDefinition.REPOSITORY_KEY.equals(
        rule.repository());
  }
}
