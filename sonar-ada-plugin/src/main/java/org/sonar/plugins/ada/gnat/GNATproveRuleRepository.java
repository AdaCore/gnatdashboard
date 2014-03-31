/****************************************************************************
 *                              Sonar Ada Plugin                            *
 *                                                                          *
 *                     Copyright (C) 2013-2014, AdaCore                     *
 *                                                                          *
 * This is free software;  you can redistribute it  and/or modify it  under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  This software is distributed in the hope  that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 ****************************************************************************/

package org.sonar.plugins.ada.gnat;

import org.sonar.plugins.ada.utils.AbstractAdaRuleRepository;

/**
 * Represent GNATcheck rule repository.
 */
public class GNATproveRuleRepository extends AbstractAdaRuleRepository {

    static final String KEY = "gnatprove";

    public GNATproveRuleRepository() {
        super(KEY);
        setName(KEY);
    }

    /**
     * Return location of GNATcheck rule repository, from resource directory.
     *
     * No default severity set in the rule repository.
     * GNATcheck rules default severity is set to MAJOR in the Sonar Ada
     * default profile.
     */
    protected String fileName() {
        return "/gnatprove.xml";
    }
}
