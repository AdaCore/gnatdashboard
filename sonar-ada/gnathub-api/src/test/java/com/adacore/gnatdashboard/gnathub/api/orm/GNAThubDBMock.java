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

import java.io.File;
import java.io.FileNotFoundException;

public class GNAThubDBMock {
  private static final String GNATHUB_TEST_DB = "gnathub-2016-11-17T200502Z.db";
  private static final String GNATHUB_SOURCE_DIR =
      "/Users/delay/_work/sandboxes/wave/x86_64-darwin/gnathub-cov/src/gnathub/src/";
  public static final String GNATHUB_MAIN = GNATHUB_SOURCE_DIR + "gnathub/gnathub.adb";

  public static File getGNAThubTestDB() throws FileNotFoundException {
    return new File(GNAThubDBMock.class.getClassLoader().getResource(GNATHUB_TEST_DB).getFile());
  }
}
