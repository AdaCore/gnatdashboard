/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.persistence;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import org.sonar.api.measures.Measure;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.Violation;
import org.sonar.plugins.ada.api.resource.AdaFile;
import org.sonar.plugins.ada.codepeer.AdaCodePeerRuleRepository;
import org.sonar.plugins.ada.codepeer.CodePeerSeverity;
import org.sonar.plugins.ada.gcov.CoverageData;
import org.sonar.plugins.ada.utils.AdaUtils;
import org.sonar.plugins.ada.utils.Pair;

public class AdaDao {

    // Map the result of query with the AdaFile object
    RowMapper resourceMapper = new RowMapper() {
        @Override
        public AdaFile mapRow(ResultSet resultSet) throws SQLException {
            AdaFile resource = new AdaFile();
            resource.setFileName(resultSet.getString("fileName"));
            resource.setDirectory(resultSet.getString("directory"));
            resource.setProject(resultSet.getString("project"));
            return resource;
        }
    };
    // Query to retrieve all project file with their
    // directory and project parent
    private final static String BASIC_RESOURCE_QUERY = "SELECT file.name as \"fileName\", "
            + "dir.name as \"directory\", prj.name as \"project\" "
            + "FROM resources file, resources dir, resources prj, "
            + "resource_trees tree_file, resource_trees tree_dir "
            + "WHERE file.kind = 2 AND dir.kind = 1  AND prj.kind = 0 "
            + "AND file.id = tree_file.child_id AND dir.id = tree_file.parent_id "
            + "AND dir.id = tree_dir.child_id  AND prj.id = tree_dir.parent_id";

    public List<AdaFile> selectAllResources() {

        return JDBCUtils.query(BASIC_RESOURCE_QUERY, resourceMapper);
    }

    public AdaFile getResourceById(final Integer resource_id) {
        String queryString = BASIC_RESOURCE_QUERY + " AND file.id = " + resource_id.toString();
        List<AdaFile> results = JDBCUtils.query(queryString, resourceMapper);
        return results.get(0);
    }

    public List<Violation> selectAllViolations() {
        String queryString = "SELECT m.data as \"msg\", r.identifier as \"rule_key\", "
                + "REPLACE(LOWER(t.name), ' ', '') as \"rule_repository\", "
                + "l.line, l.resource_id, c.label as \"category\" "
                + "FROM lines_messages lm, rules r, tools t, lines l,"
                + "messages m  LEFT OUTER JOIN categories c ON m.category_id = c.id "
                + "WHERE m.rule_id = r.id "
                + "AND lm.message_id = m.id "
                + "AND r.tool_id = t.id "
                + "AND lm.line_id = l.id "
                + "AND r.kind = 0";

        RowMapper violationMapper = new RowMapper() {
            @Override
            public Violation mapRow(ResultSet resultSet) throws SQLException {
                AdaDao dao = new AdaDao();
                String ruleKey = resultSet.getString("rule_key");

                // Mangage rule with category (ex: CodePeer messsages)
                if (resultSet.getString("category") != null) {
                    String[] severity_category = resultSet.getString("category")
                            .split(AdaCodePeerRuleRepository.RULE_KEY_SEPARATOR);

                    // Handle only categories with following fromat: category__severity
                    if (severity_category.length > 1){
                        String severity = CodePeerSeverity.valueOf(severity_category[0]).getSonarSeverity();
                        ruleKey = severity + AdaCodePeerRuleRepository.RULE_KEY_SEPARATOR
                                + severity_category[1] + AdaCodePeerRuleRepository.RULE_KEY_SEPARATOR
                                + ruleKey;
                    } else {
                        ruleKey = resultSet.getString("category")
                                + AdaCodePeerRuleRepository.RULE_KEY_SEPARATOR
                                + ruleKey;
                    }
                }

                AdaUtils.LOG.info("Rule key: {}", ruleKey);

                Rule rule = Rule.create(resultSet.getString("rule_repository"), ruleKey);
                AdaFile resource = dao.getResourceById(resultSet.getInt("resource_id"));
                Violation violation = Violation.create(rule, resource)
                        .setLineId(resultSet.getInt("line"))
                        .setMessage(resultSet.getString("msg"));
                return violation;
            }
        };
        return JDBCUtils.query(queryString, violationMapper);
    }

    public List<Pair<AdaFile, Measure>> selectMeasureForTool(String toolName) {
        String queryString = "SELECT m.data as value, r.identifier as metric_key, "
                + "REPLACE(t.name, ' ', '') as \"tool\", rm.resource_id "
                + "FROM resources_messages rm, rules r, tools t, messages m "
                + "WHERE m.rule_id = r.id "
                + "AND rm.message_id = m.id "
                + "AND r.tool_id = t.id "
                + "AND m.rule_id = r.id "
                + "AND r.kind = 1 "
                + "AND UPPER(t.name) = UPPER('" + toolName + "');";

        RowMapper measureMapper = new RowMapper() {
            @Override
            public Pair<AdaFile, Measure> mapRow(ResultSet resultSet) throws SQLException {
                AdaDao dao = new AdaDao();
                AdaFile file = dao.getResourceById(resultSet.getInt("resource_id"));
                Measure measure = new Measure(resultSet.getString("metric_key"));
                measure.setValue(resultSet.getDouble("value"));
                Pair fileMeasure = new Pair(file, measure);
                return fileMeasure;
            }
        };

        return JDBCUtils.query(queryString, measureMapper);
    }

    public List<CoverageData> getCoverageData() {
        String queryString = "SELECT rs.id as resource_id, l.line as line, "
                           +         "m.data as hits "
                           + "FROM resources rs, lines_messages lm, "
                           +       "messages m, tools t, rules r, lines l "
                           + "WHERE t.id = r.tool_id "
                           + "AND r.id = m.rule_id "
                           + "AND m.id = lm.message_id "
                           + "AND lm.line_id = l.id "
                           + "AND l.resource_id = rs.id "
                           + "AND UPPER(t.name) = UPPER('Gcov') "
                           + "ORDER BY (rs.id);";

        RowMapper measureMapper = new RowMapper() {
            @Override
            public CoverageData mapRow(ResultSet resultSet) throws SQLException {
                AdaDao dao = new AdaDao();
                AdaFile file = dao.getResourceById(resultSet.getInt("resource_id"));
                CoverageData data = new CoverageData(file, resultSet.getInt("line"),
                                                           resultSet.getInt("hits"));

                return data;
            }
        };

        return JDBCUtils.query(queryString, measureMapper);
    }
}