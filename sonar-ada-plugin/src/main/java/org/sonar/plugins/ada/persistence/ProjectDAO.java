/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2014-2015, AdaCore
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

package org.sonar.plugins.ada.persistence;

import lombok.extern.slf4j.Slf4j;
import org.sonar.api.measures.Measure;
import org.sonar.api.resources.File;
import org.sonar.api.resources.Project;
import org.sonar.api.rules.Rule;
import org.sonar.plugins.ada.codepeer.CodePeerRulesDefinition;
import org.sonar.plugins.ada.codepeer.CodePeerSeverity;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Properties;

/**
 * Data Access Object for the content of the project database.
 */
@Slf4j
public class ProjectDAO {
  private final String dbUrl;
  private final Project project;
  private final Connector connector;
  private final Properties srcMapping;

  /**
   * Project DAO constructor. Initializes the database connector with the URL.
   *
   * @param project The project being analyzed.
   * @param dbUrl The URL pointing to the SQLite database to connect to.
   * @param srcMapping Source file mapping.
   */
  public ProjectDAO(final Project project, final String dbUrl,
                    final Properties srcMapping) {
    this.dbUrl = dbUrl;
    this.project = project;
    this.connector = new Connector(this.dbUrl);
    this.srcMapping = srcMapping;
  }

  /**
   * Returns the path to the file.
   *
   * Use source mapping to compute the local cached file path. If it fails,
   * uses the original path as fallback.
   *
   * @param original The original path for the file.
   * @return The mapped path, or the original one if not mapped.
   */
  private String getPath(final String original) {
    final String path = srcMapping.getProperty(original, original);

    if (path.equals(original)) {
      log.warn("No source mapping found for: {}", path);
      // Do not return so that we fallback using the original file if
      // available.
    }

    return path;
  }

  /**
   * Custom row mapper for Sonar {@code File}s.
   */
  private final RowMapper<File> fileRowMapper = new RowMapper<File>() {
    @Override
    public File mapRow(ResultSet resultSet) throws SQLException {
      final String path = getPath(resultSet.getString("path"));
      final File file = File.fromIOFile(new java.io.File(path), project);

      if (file == null) {
        log.debug("File excluded from analysis closure: {}", path);
        return null;
      }

      log.trace("File @ {}", path);
      return file;
    }
  };

  /**
   * Custom row mapper for pairs file/measure.
   */
  private final RowMapper<MeasureRecord> measureRowMapper =
      new RowMapper<MeasureRecord>() {
        @Override
        public MeasureRecord mapRow(ResultSet resultSet) throws SQLException {
          final String path = getPath(resultSet.getString("path"));
          final String key = resultSet.getString("key");
          final Double value = resultSet.getDouble("value");
          final File file = File.fromIOFile(new java.io.File(path), project);

          if (file == null) {
            log.trace("File excluded from analysis closure: {}", path);
            return null;
          }

          final Measure measure = new Measure(key);
          measure.setValue(value);
          log.trace("Measure @ {}: {} = {}", new Object[]{path, key, value});

          return new MeasureRecord(file, measure);
        }
      };

  /**
   * Custom row mapper for coverage.
   */
  private final RowMapper<CoverageRecord> coverageRowMapper =
      new RowMapper<CoverageRecord>() {
        @Override
        public CoverageRecord mapRow(ResultSet resultSet) throws SQLException {
          final String path = getPath(resultSet.getString("path"));
          final Integer lineNo = resultSet.getInt("line_no");
          final Integer hits = resultSet.getInt("hits");
          final File file = File.fromIOFile(new java.io.File(path), project);

          if (file == null) {
            log.trace("File excluded from analysis closure: {}", path);
            return null;
          }

          log.trace("Coverage @ {}:{} -> {}", new Object[]{path, lineNo, hits});
          return new CoverageRecord(file, lineNo, hits);
        }
      };

  /**
   * Custom row mapper for issues.
   * ??? Review this method and remove deprecated code.
   */
  private final RowMapper<IssueRecord> issueRowMapper = new RowMapper<IssueRecord>() {
    @Override
    public IssueRecord mapRow(ResultSet resultSet) throws SQLException {
      final String path = getPath(resultSet.getString("path"));
      final int lineNo = resultSet.getInt("line_no");
      final String message = resultSet.getString("message");
      final String toolName = resultSet.getString("tool_name");
      final String key = resultSet.getString("key");
      final String category = resultSet.getString("category");

      log.trace(
          "Generate Rule Key from key=\"{}\" and category=\"{}\" -> \"{}\"",
          resultSet.getString("key"), resultSet.getString("category"), key);

      final File file = File.fromIOFile(new java.io.File(path), project);

      if (file == null) {
        log.trace("File excluded from analysis closure: {}", path);
        return null;
      }

      final Rule rule = Rule.create(toolName, key);
      log.trace("({}) @ {}:{} -> {}:{}", toolName, path, lineNo, key, message);

      String severity = null;
      if (CodePeerRulesDefinition.REPOSITORY_KEY.equals(toolName)) {
        severity =CodePeerSeverity.valueOf(
                category.toUpperCase()).getSonarSeverity();
      }
      return new IssueRecord(file, lineNo, message, rule, severity);
    }
  };

  /**
   * @return The complete list of files in the project.
   */
  public final List<File> getFiles() {
    return connector.query(SQLQueries.allResources(), fileRowMapper);
  }

  /**
   * Fetches one file from the project.
   *
   * @param resourceId The ID of the resource.
   * @return The file associated with that resource ID.
   */
  public final File getFileById(final Integer resourceId) {
    final List<File> files = connector.query(
        SQLQueries.resourceById(resourceId), fileRowMapper);

    if (files.size() > 1) {
      log.error("Found {} resources for ID = {} (expected 1)",
          files.size(), resourceId);
    }

    return files.isEmpty() ? null : files.get(0);
  }

  /**
   * Fetches all measures produced by the given tool.
   *
   * @param toolName The tool name used to filter measures.
   * @return The list of measures generated by that tool.
   */
  public final List<MeasureRecord> getMeasuresByTool(String toolName) {
    return connector.query(
        SQLQueries.measuresByTool(toolName), measureRowMapper);
  }

  /**
   * Fetches all coverage information produced by the given tool.
   *
   * @param toolName The tool name used to filter coverage.
   * @return The list of coverage records generated by that tool.
   */
  public final List<CoverageRecord> getCoverageByTool(String toolName) {
    return connector.query(
        SQLQueries.coverageByTool(toolName), coverageRowMapper);
  }

  /**
   * @return The complete list of issues in the project.
   */
  public final List<IssueRecord> getIssues() {
    return connector.query(SQLQueries.allIssues(), issueRowMapper);
  }
}
