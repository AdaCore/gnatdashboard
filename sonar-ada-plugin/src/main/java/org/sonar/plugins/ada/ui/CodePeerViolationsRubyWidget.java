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

package org.sonar.plugins.ada.ui;

import org.sonar.api.web.*;

@UserRole(UserRole.USER)
@Description("Shows Codepeer messages, per severity and category")
@WidgetCategory("Rules")
public class CodePeerViolationsRubyWidget
    extends AbstractRubyTemplate implements RubyRailsWidget
{
  public String getId() {
    return "CodepeerViolations";
  }

  public String getTitle() {
    return "Codepeer Messages";
  }

  @Override
  protected String getTemplatePath() {
    return "/org/sonar/plugins/ada/ui/codepeer_violations.html.erb";
  }
}
