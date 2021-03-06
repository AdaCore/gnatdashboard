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
import java.util.Optional;

import static org.fest.assertions.Assertions.assertThat;

public class CoverageDAOTest {
  private LineHits getLine (int lineno, FileCoverage coverage) {
    for (LineHits hits : coverage.hits){
      if (hits.line == lineno) {
        return hits;
      }
    }
    return null;
  }

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
    assertThat(coverage.hits).isNotEmpty();
    assertThat(coverage.hits.size()).isEqualTo(15);
    assertThat(coverage.hits.get(0).line).isEqualTo(11);
    assertThat(coverage.hits.get(0).count).isEqualTo(1);
    assertThat(coverage.hits.get(12).line).isEqualTo(48);
    assertThat(coverage.hits.get(12).count).isEqualTo(1);

    Optional.ofNullable(getLine(11, coverage))
            .ifPresent(hit -> assertThat(hit.count).isEqualTo(1));
    Optional.ofNullable(getLine(48, coverage))
            .ifPresent(hit -> assertThat(hit.count).isEqualTo(1));
  }
}
