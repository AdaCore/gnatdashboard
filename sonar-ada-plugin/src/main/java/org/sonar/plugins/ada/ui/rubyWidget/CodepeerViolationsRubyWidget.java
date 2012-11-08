/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.ui.rubyWidget;

import org.sonar.api.web.UserRole;
import org.sonar.api.web.AbstractRubyTemplate;
import org.sonar.api.web.Description;
import org.sonar.api.web.RubyRailsWidget;
import org.sonar.api.web.WidgetCategory;

@UserRole(UserRole.USER)
@Description("Show how to use Ruby Widget API")
@WidgetCategory("Rules")

public class CodepeerViolationsRubyWidget extends AbstractRubyTemplate implements RubyRailsWidget {

  public String getId() {
    return "TestOne";
  }

  public String getTitle() {
    return "TestOne";
  }

  @Override
  protected String getTemplatePath() {
    return "/org/sonar/plugins/ada/ui/rubyWidget/codepeer_violations.html.erb";
  }
}
