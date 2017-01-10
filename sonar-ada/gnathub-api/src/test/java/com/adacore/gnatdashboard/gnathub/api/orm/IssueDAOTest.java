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

import lombok.Cleanup;
import org.junit.Test;

import java.io.File;

import static org.fest.assertions.Assertions.assertThat;

public class IssueDAOTest {
  @Test
  public void getIssuesForFile() throws Exception {
    final File db = GNAThubDBMock.getGNAThubTestDB();
    assertThat(db.exists()).isTrue();

    @Cleanup("closeConnection") final Connector connector = new Connector(db);
    connector.openConnection();

    final IssueDAO issueDAO = new IssueDAO(connector);
    final FileIssues issues = issueDAO.getIssuesForFile(GNAThubDBMock.GNATHUB_MAIN);
    assertThat(issues).isNotNull();
    assertThat(issues.fromToolIgnoreCase("gnatcheck")).isNotEmpty();
    assertThat(issues.fromToolIgnoreCase("gnatcheck").size()).isEqualTo(2);
    assertThat(issues.fromToolIgnoreCase("codepeer")).isNotEmpty();
    assertThat(issues.fromToolIgnoreCase("codepeer").size()).isEqualTo(1);
  }
}
