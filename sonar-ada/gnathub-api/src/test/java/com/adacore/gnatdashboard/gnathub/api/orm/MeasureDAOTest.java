/*
 * GNATdashboard
 * Copyright (C) 2016-2018, AdaCore
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
    assertThat(measures.path).isEqualTo(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(measures.asInt(GNATmetricMetrics.ALL_LINES)).isEqualTo(50);
    assertThat(measures.asInt(GNATmetricMetrics.BLANK_LINES)).isEqualTo(12);
    assertThat(measures.asInt(GNATmetricMetrics.CODE_LINES)).isEqualTo(36);
    assertThat(measures.asInt(GNATmetricMetrics.EOL_COMMENTS)).isEqualTo(0);
    assertThat(measures.asInt(GNATmetricMetrics.COMMENT_LINES)).isEqualTo(2);
    assertThat(measures.asDouble(GNATmetricMetrics.COMMENT_PERCENTAGE)).isEqualTo(5.26);
    assertThat(measures.asDouble(GNATmetricMetrics.CYCLOMATIC_COMPLEXITY)).isEqualTo(2.00);
    assertThat(measures.asDouble(GNATmetricMetrics.ESSENTIAL_COMPLEXITY)).isEqualTo(1.00);
    assertThat(measures.asDouble(GNATmetricMetrics.STATEMENT_COMPLEXITY)).isEqualTo(2.00);
    assertThat(measures.asDouble(GNATmetricMetrics.EXPRESSION_COMPLEXITY)).isEqualTo(0.00);
    // NOTE: GNATmetric reports MAX_LOOP_NESTING for files as a double because it is actually the
    // *average* of all MAX_LOOP_NESTING results reported in that file.
    assertThat(measures.asDouble(GNATmetricMetrics.MAX_LOOP_NESTING)).isEqualTo(1.0);
  }
}
