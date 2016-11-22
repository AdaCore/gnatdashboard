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

import org.junit.Test;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;

public class CoverageDAOTest {
  @Test
  public void getCoverageForFile() throws Exception {
    final File db = GNAThubDBMock.getGNAThubTestDB();
    assertThat(db.exists()).isTrue();
    final CoverageDAO coverageDAO = new CoverageDAO(new Connector(db));
    final FileCoverage coverage = coverageDAO.getCoverageForFile(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(coverage).isNotNull();
    assertThat(coverage.getPath()).isEqualTo(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(coverage.getHits()).isNotEmpty();
    assertThat(coverage.getHits().size()).isEqualTo(59);
    assertThat(coverage.getHits().get(0).getLine()).isEqualTo(22);
    assertThat(coverage.getHits().get(0).getCount()).isEqualTo(34);
    assertThat(coverage.getHits().get(12).getLine()).isEqualTo(69);
    assertThat(coverage.getHits().get(12).getCount()).isEqualTo(0);
  }
}
