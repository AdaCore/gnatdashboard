/*
 * GNATdashboard
 * Copyright (C) 2017-2019, AdaCore
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

package org.sonar.plugins.ada.sensors;

import com.adacore.gnatdashboard.gnathub.api.orm.ExemptedViolations;
import com.adacore.gnatdashboard.gnathub.api.orm.FileExemptedViolations;
import com.adacore.gnatdashboard.gnathub.api.orm.FileIssues;
import com.adacore.gnatdashboard.gnathub.api.orm.Issue;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.batch.fs.InputFile;
import org.sonar.api.batch.rule.ActiveRule;
import org.sonar.api.batch.rule.Severity;
import org.sonar.api.batch.sensor.SensorContext;
import org.sonar.api.batch.sensor.issue.NewIssue;
import org.sonar.api.batch.sensor.issue.NewIssueLocation;
import org.sonar.api.rule.RuleKey;
import org.sonar.plugins.ada.GNAThub;
import org.sonar.plugins.ada.rules.CodePeerSeverity;
import org.sonar.plugins.ada.rules.SPARK2014Severity;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Sensor that collect generic issues known to GNAThub.
 *
 * Lists all issues stored within GNAThub, in a tool-agnostic manner, and saves them to SonarQube.
 * Saves among other tools issues from CodePeer and GNATcheck.
 */
@Slf4j
public class GNAThubIssueSensor extends MainFilesSensor {
  private static final String CODEPEER = "codepeer";
  private static final String GNATCHECK = "gnatcheck";
  private static final String SPARK2014 = "spark2014";

  private static final String SUPPRESSED = "suppressed";
  private final List<Issue> suppressedIssues = new ArrayList<>();
  private final List<Issue> exemptedIssues = new ArrayList<>();

  private final Table<String, String, Integer> missingRules = HashBasedTable.create();

  @Override
  public String getName() {
    return "GNAThub Issues Import";
  }

  private Severity getSonarSeverity(final Issue issue) {
    if (CODEPEER.equalsIgnoreCase(issue.getTool())) {
      try {
        return CodePeerSeverity.valueOf(issue.getCategory().toUpperCase()).getSonarSeverity();
      } catch (final IllegalArgumentException why) {
        log.warn("Unsupported CodePeer severity \"{}\" - defaults to MAJOR", issue.getCategory());
        return Severity.MAJOR;
      }
    }
    if (SPARK2014.equalsIgnoreCase(issue.getTool())) {
      try {
        return SPARK2014Severity.valueOf(issue.getCategory().toUpperCase()).getSonarSeverity();
      } catch (final IllegalArgumentException why) {
        log.warn("Unsupported SPARK2014 severity \"{}\" - defaults to MAJOR", issue.getCategory());
        return Severity.MAJOR;
      }
    }
    return null; // A null value means to use severity configured in the quality profile.
  }

  @Override
  public void tearDown() {
    if (exemptedIssues.size() != 0) {
      // Log the number of GNATcheck exempted violations (GNATcheck "exempted" property).
      log.warn(" {} GNATcheck exempted violations have been found " +
               " (see GNATcheck's report \"Exempted Coding Standard Violations\" section).",
               exemptedIssues.size());
      log.warn(" Resolutions needs to be set manually for GNATcheck exemptions!");
      exemptedIssues.clear();
    }

    if (suppressedIssues.size() != 0) {
      // Log the number of silenced issues (CodePeer "suppressed").
      log.info("Silenced {} issues (see CodePeer's \"suppressed\")", suppressedIssues.size());
      suppressedIssues.clear();
    }
    // Log the rules that were unknown or inactive.
    for (final Table.Cell<String, String, Integer> cell : missingRules.cellSet()) {
      log.warn("Unknown or inactive rule \"{}\" from repository \"{}\" ({} times)",
          new Object[]{ cell.getColumnKey(), cell.getRowKey(), cell.getValue() });
    }
    missingRules.clear();
  }

  @Override
  public void forInputFile(final SensorContext context, final GNAThub gnathub, final InputFile file)
  {
    final FileIssues issues = gnathub.getIssues().forFile(file.uri().getPath());
    final FileExemptedViolations exempted = gnathub.getExemptedViolations().forFile(file.uri().getPath());

    // Defensive programming
    if (issues == null) {
      log.warn("Skipping: {}", file.uri().getPath());
      return;
    }

    for (final Issue issue : issues.getIssues()) {
      if (CODEPEER.equalsIgnoreCase(issue.getTool()) &&
          SUPPRESSED.equalsIgnoreCase(issue.getCategory())) {
        suppressedIssues.add(issue);
        continue; // Silence CodePeer's SUPPRESSED rules.
      }
      // Locate the rule in the given rule repository.
      final ActiveRule rule =
          context.activeRules().find(RuleKey.of(issue.getTool(), issue.getKey()));
      if (rule == null) {
        missingRules.put(issue.getTool(), issue.getKey(),
            Optional.ofNullable(
                missingRules.get(issue.getTool(), issue.getKey())).orElse(0) + 1);
        continue;
      }
      // Create the issue and save it.
      final NewIssue newIssue = context.newIssue()
          .forRule(rule.ruleKey())
          .overrideSeverity(getSonarSeverity(issue));
      final NewIssueLocation location = newIssue.newLocation()
          .message(issue.getMessage())
          .on(file)
          .at(file.selectLine(issue.getLine()));
      newIssue.at(location).save();
    }

    // Store and log GNATcheck exempted violations if any
    if (exempted != null) {
      // Dump GNATcheck exempted violations list
      if (exempted.getExemptedViolations().size() != 0) {
        log.warn(" GNATcheck exempted violation found for {}:", file.uri().getPath());
        for (final Issue violation : exempted.getExemptedViolations()) {
          exemptedIssues.add(violation);
          log.warn("   At line " + violation.getLine() + ": " + violation.getMessage());
        }
      }
    }
  }
}
