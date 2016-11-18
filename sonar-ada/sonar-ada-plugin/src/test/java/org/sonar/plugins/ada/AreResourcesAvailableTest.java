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

package org.sonar.plugins.ada;

import org.junit.Test;
import org.sonar.plugins.ada.rules.CodePeerRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.GNATcheckRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.GNATcoverageRulesDefinitionXmlLoader;
import org.sonar.plugins.ada.rules.CustomRulesDefinitionXmlLoader;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;

import static org.fest.assertions.Assertions.assertThat;

public class AreResourcesAvailableTest {
    /**
     * Given a resource name, test its existence.
     *
     * @param name The name of the resource, eg. "gnatcheck.xml".
     * @return {@code true} if the resource exists, {@code false} otherwise.
     */
    private void existsResource(final String name) throws URISyntaxException {
        final URL resource = CustomRulesDefinitionXmlLoader.class.getResource(name);
        assertThat(resource).isNotNull();

        final File resourceFile = new File(resource.toURI());
        assertThat(resourceFile).isNotNull();
        assertThat(resourceFile.exists() && resourceFile.isFile()).isTrue();
    }

    @Test
    public void existsGNATcheckXMLRulesDefinition() throws URISyntaxException {
        existsResource(GNATcheckRulesDefinitionXmlLoader.RULES_DEFINITION_FILE);
    }

    @Test
    public void existsGNATcovXMLRulesDefinition() throws URISyntaxException {
        existsResource(GNATcoverageRulesDefinitionXmlLoader.RULES_DEFINITION_FILE);
    }

    @Test
    public void existsCodePeerXMLRulesDefinition() throws URISyntaxException {
        existsResource(CodePeerRulesDefinitionXmlLoader.RULES_DEFINITION_FILE);
    }

    @Test
    public void existsAdaXMLRulesProfile() throws URISyntaxException {
        existsResource(AdaDefaultProfile.RULES_PROFILE_FILE);
    }
}
