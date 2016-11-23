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

package com.adacore.gnatdashboard.gnathub.api.orm;

import com.adacore.gnatdashboard.gnathub.api.orm.constant.GNATmetricMetrics;
import lombok.Cleanup;
import org.junit.Test;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;

public class MeasureDAOTest {
  @Test
  public void getMeasuresForFile() throws Exception {
    final File db = GNAThubDBMock.getGNAThubTestDB();
    assertThat(db.exists()).isTrue();

    @Cleanup("closeConnection") final Connector connector = new Connector(db);
    connector.openConnection();

    final MeasureDAO measureDAO = new MeasureDAO(connector);
    final FileMeasures measures = measureDAO.getMeasuresForFile(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(measures).isNotNull();
    assertThat(measures.getPath()).isEqualTo(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(measures.getMeasures().asInt(GNATmetricMetrics.ALL_LINES)).isEqualTo(177);
    assertThat(measures.getMeasures().asInt(GNATmetricMetrics.BLANK_LINES)).isEqualTo(33);
    assertThat(measures.getMeasures().asInt(GNATmetricMetrics.CODE_LINES)).isEqualTo(93);
    assertThat(measures.getMeasures().asInt(GNATmetricMetrics.EOL_COMMENTS)).isEqualTo(0);
    assertThat(measures.getMeasures().asInt(GNATmetricMetrics.COMMENT_LINES)).isEqualTo(51);
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.COMMENT_PERCENTAGE)).isEqualTo(35.41);
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.CYCLOMATIC_COMPLEXITY)).isEqualTo(1.50);
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.ESSENTIAL_COMPLEXITY)).isEqualTo(1.);
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.STATEMENT_COMPLEXITY)).isEqualTo(1.39);
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.EXPRESSION_COMPLEXITY)).isEqualTo(0.1);
    // NOTE: GNATmetric reports MAX_LOOP_NESTING for files as a double because it is actually the
    // *average* of all MAX_LOOP_NESTING results reported in that file.
    assertThat(measures.getMeasures().asDouble(GNATmetricMetrics.MAX_LOOP_NESTING)).isEqualTo(0.);
  }
}
