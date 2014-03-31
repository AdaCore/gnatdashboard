/****************************************************************************
 *                              Sonar Ada Plugin                            *
 *                                                                          *
 *                     Copyright (C) 2013-2014, AdaCore                     *
 *                                                                          *
 * This is free software;  you can redistribute it  and/or modify it  under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  This software is distributed in the hope  that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 ****************************************************************************/

package org.sonar.plugins.ada;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.resources.Project;
import org.sonar.api.rule.RuleKey;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.RuleQuery;
import org.sonar.plugins.ada.persistence.IssueRecord;
import org.sonar.plugins.ada.utils.AbstractAdaSensor;

@Slf4j
@AllArgsConstructor
public class AdaIssueSensor extends AbstractAdaSensor {
  private AdaProjectContext adaContext;

  private RuleFinder ruleFinder;
  private ResourcePerspectives resourcePerspectives;

  @Override
  public void analyse(Project project, SensorContext context) {
    log.info("Collecting all issues from DB...");

    for (IssueRecord ai : adaContext.getDao().getIssues()) {
      log.debug("Issue: \"{}\" @ {}", ai.getMessage(), ai.getFile().getName());

      // Try to find rule in the given rule repository
      RuleQuery ruleQuery = RuleQuery.create()
          .withRepositoryKey(ai.getRule().getRepositoryKey())
          .withKey(ai.getRule().getKey());

      log.debug("Rule repo: \"{}\" -- Rule key: \"{}\"",
          ai.getRule().getRepositoryKey(), ai.getRule().getKey());

      Rule rule = ruleFinder.find(ruleQuery);

      if (rule == null) {
        log.warn("{}: no such rule, skipping", ai.getRule().getKey());
        continue;
      }

      Issuable issuable = resourcePerspectives.as(Issuable.class, ai.getFile());

      if (issuable == null) {
        log.warn("{}: no such file, skipping", ai.getFile().getLongName());
        continue;
      }

      Issue issue = issuable.newIssueBuilder()
          .ruleKey(RuleKey.of(
              ai.getRule().getRepositoryKey(),
              ai.getRule().getKey()))
          .line(ai.getLine())
          .message(ai.getMessage())
          .build();

      issuable.addIssue(issue);
    }
  }
}
