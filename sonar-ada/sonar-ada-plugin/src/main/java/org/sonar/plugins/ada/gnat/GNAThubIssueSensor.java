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

package org.sonar.plugins.ada.gnat;

import com.adacore.gnatdashboard.gnathub.api.orm.IssueRecord;
import lombok.AllArgsConstructor;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.rule.Severity;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.Resource;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.RuleQuery;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.utils.ResourceUtils;

import java.util.Collection;

/**
 * Sensor that collect generic issues stored though GNAThub.
 *
 * Lists all issues stored within GNAThub, in a tool-agnostic manner. Among
 * other tools, it can read issues from CodePeer and GNATcheck.
 */
@Slf4j
@ToString
@AllArgsConstructor
public class GNAThubIssueSensor implements Sensor {
  private final FileSystem fs;
  private final AdaProjectContext adaContext;
  private final RuleFinder ruleFinder;
  private final ResourcePerspectives perspective;

  @Override
  public boolean shouldExecuteOnProject(final Project project) {
    return fs.hasFiles(fs.predicates().and(
        fs.predicates().hasType(InputFile.Type.MAIN),
        fs.predicates().hasLanguage(Ada.KEY)));
  }

  @Override
  public void analyse(final Project project, final SensorContext context) {
    log.info("Loading issues from GNAThub (provided by GNAT tools)");

    if (!adaContext.isDAOLoaded()) {
      log.error("GNAThub db not loaded, cannot list project Issues");
      return;
    }

    final Collection<Resource> scope = ResourceUtils.expandChildren(project, context);

    for (final IssueRecord ai : adaContext.getDao().getIssues()) {
      // Check that the resource is from the correct project
      // ??? Augment SQL query to filter out resources per project
      final Resource resource = ai.getFile();

      if (!scope.contains(resource)) {
        log.trace("{}: not from project: {}", resource.getLongName(), project.getName());
        continue;
      }

      final Issuable issuable = perspective.as(Issuable.class, resource);
      if (issuable == null) {
        log.warn("{}: no such file", ai.getFile().getLongName());
        continue;
      }

      // Locate the rule in the given rule repository
      final RuleQuery ruleQuery = RuleQuery.create()
          .withRepositoryKey(ai.getRule().getRepositoryKey())
          .withKey(ai.getRule().getKey());
      final Rule rule = ruleFinder.find(ruleQuery);
      if (rule == null) {
        log.warn("could not find rule \"{}:{}\"",
            ai.getRule().getRepositoryKey(), ai.getRule().getKey());
        continue;
      }

      log.debug("({}:{}) '{}:{}' rule violation detected", new Object[]{
          ai.getPath(), ai.getLine(), ai.getRule().getRepositoryKey(), rule.getKey()
      });

      final Issue issue = issuable.newIssueBuilder()
          .ruleKey(rule.ruleKey())
          .line(ai.getLine())
          .message(ai.getMessage())
          .severity(ai.getSeverity() != null ? ai.getSeverity() : Severity.MAJOR.toString())
          .build();
      issuable.addIssue(issue);
    }
  }
}
