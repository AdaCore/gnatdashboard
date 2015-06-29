/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2013-2015, AdaCore
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.Sensor;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.batch.fs.FileSystem;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.component.ResourcePerspectives;
import org.sonar.api.issue.Issuable;
import org.sonar.api.issue.Issue;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleFinder;
import org.sonar.api.rules.RuleQuery;
import org.sonar.plugins.ada.AdaProjectContext;
import org.sonar.plugins.ada.lang.Ada;
import org.sonar.plugins.ada.persistence.IssueRecord;

/**
 * Sensor that collect generic issues stored though GNAThub.
 *
 * Lists all issues stored within GNAThub, in a tool-agnostic manner. Among
 * other tools, it can read issues from CodePeer and GNATcheck.
 */
@Slf4j
@AllArgsConstructor
public class GNAThubIssueSensor implements Sensor {
    private final FileSystem fs;
    private final AdaProjectContext adaContext;
    private final RuleFinder ruleFinder;
    private final ResourcePerspectives resourcePerspectives;

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

        for (final IssueRecord ai : adaContext.getDao().getIssues()) {
            // Locate the rule in the given rule repository
            RuleQuery ruleQuery = RuleQuery.create()
                    .withRepositoryKey(ai.getRule().getRepositoryKey())
                    .withKey(ai.getRule().getKey());
            Rule rule = ruleFinder.find(ruleQuery);

            if (rule == null) {
                log.warn("could not find rule \"{}:{}\"",
                        ai.getRule().getRepositoryKey(), ai.getRule().getKey());
                continue;
            }
            log.debug("({}:{}) '{}:{}' rule violation detected",
                ai.getFile().getName(), ai.getLine(),
                ai.getRule().getRepositoryKey(), rule.getKey());

            final Issuable issuable =
                resourcePerspectives.as(Issuable.class, ai.getFile());

            if (issuable == null) {
                log.warn("{}: no such file", ai.getFile().getLongName());
                continue;
            }

            final Issue issue = issuable.newIssueBuilder()
                    .ruleKey(rule.ruleKey())
                    .line(ai.getLine())
                    .message(ai.getMessage())
                    .severity(ai.getSeverity())
                    .build();

            issuable.addIssue(issue);
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
