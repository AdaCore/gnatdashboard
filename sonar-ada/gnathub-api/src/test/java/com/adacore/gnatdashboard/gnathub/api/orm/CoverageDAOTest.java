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

import lombok.Cleanup;
import org.junit.Test;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;

public class CoverageDAOTest {
  @Test
  public void getCoverageForFile() throws Exception {
    final File db = GNAThubDBMock.getGNAThubTestDB();
    assertThat(db.exists()).isTrue();

    @Cleanup("closeConnection") final Connector connector = new Connector(db);
    connector.openConnection();

    final CoverageDAO coverageDAO = new CoverageDAO(connector);
    final FileCoverage coverage = coverageDAO.getCoverageForFile(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(coverage).isNotNull();
    assertThat(coverage.path).isEqualTo(GNAThubDBMock.GNATHUB_MAIN);
    // TO DO : updated the DB with gcov results (commented until)
//    assertThat(coverage.hits).isNotEmpty();
//    assertThat(coverage.hits.size()).isEqualTo(59);
//    assertThat(coverage.hits.get(0).line).isEqualTo(22);
//    assertThat(coverage.hits.get(0).count).isEqualTo(34);
//    assertThat(coverage.hits.get(12).line).isEqualTo(69);
//    assertThat(coverage.hits.get(12).count).isEqualTo(0);
  }
}
