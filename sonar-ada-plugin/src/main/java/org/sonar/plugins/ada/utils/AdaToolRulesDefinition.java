/*
 * Sonar Ada Plugin (GNATdashboard)
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

package org.sonar.plugins.ada.utils;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.api.server.rule.RulesDefinitionXmlLoader;
import org.sonar.plugins.ada.lang.Ada;

import java.io.InputStreamReader;

/**
 * Base class for rules definitions.
 *
 * Provides the common mechanism to load a rules definition XML file from the
 * project resources.
 */
@Slf4j
@AllArgsConstructor
public abstract class AdaToolRulesDefinition implements RulesDefinition {
  private final RulesDefinitionXmlLoader xmlLoader;

  /**
   * @return The repository key.
   */
  public abstract String getRepositoryKey();

  /**
   * @return The name of the tool (eg. "GNATcheck").
   */
  public abstract String getToolName();

  /**
   * @return The path to the XML file containing the rules definition.
   */
  public abstract String getRulesDefinitionXMLFile();

  @Override
  public void define(Context context) {
    final NewRepository repository = context
        .createRepository(getRepositoryKey(), Ada.KEY)
        .setName(String.format("{} rules", getToolName()));

    final String definitions = getRulesDefinitionXMLFile();

    log.debug("Loading XML definition file: {}", definitions);
    final InputStreamReader reader = new InputStreamReader(
        AdaToolRulesDefinition.class.getResourceAsStream(definitions));

    xmlLoader.load(repository, reader);
    repository.done();
  }
}
