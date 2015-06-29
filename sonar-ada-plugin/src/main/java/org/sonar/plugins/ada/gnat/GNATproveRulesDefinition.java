/**
 * Sonar Ada Plugin (GNATdashboard)
 * Copyright (C) 2013-2015, AdaCore
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

package org.sonar.plugins.ada.gnat;

import org.sonar.api.server.rule.RulesDefinitionXmlLoader;
import org.sonar.plugins.ada.utils.AdaToolRulesDefinition;

/**
 * GNATprove rules definition.
 *
 * Loads the XML file containing each rule definition and register them in
 * SonarQube as part of a new rule repository.
 */
public class GNATproveRulesDefinition extends AdaToolRulesDefinition {
    public GNATproveRulesDefinition(RulesDefinitionXmlLoader xmlLoader) {
        super(xmlLoader);
    }

    public static final String REPOSITORY_KEY = "gnatprove";
    public static final String RULES_DEFINITION_FILE = "/gnatprove.xml";

    @Override
    public String getRepositoryKey() {
        return REPOSITORY_KEY;
    }

    @Override
    public String getToolName() {
        return "GNATprove";
    }

    @Override
    public String getRulesDefinitionXMLFile() {
        return RULES_DEFINITION_FILE;
    }
}
