/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.ui.gwt;

import org.sonar.api.resources.Resource;
import org.sonar.api.web.UserRole;
import org.sonar.api.web.DefaultTab;
import org.sonar.api.web.NavigationSection;
import org.sonar.api.web.ResourceLanguage;
import org.sonar.api.web.ResourceQualifier;
import org.sonar.api.web.GwtPage;
import org.sonar.plugins.ada.Ada;
import org.sonar.plugins.ada.ui.gwt.client.GnatMetricViewer;

/**
 * Defines the GnatMetric's tab view.
 * GwtGnatMetrics contains the implementation of the view
 */
@ResourceQualifier({Resource.QUALIFIER_CLASS,Resource.QUALIFIER_FILE})
@NavigationSection(NavigationSection.RESOURCE_TAB)
@ResourceLanguage(Ada.KEY)
@DefaultTab
@UserRole(UserRole.CODEVIEWER)
public class GnatMetricViewerDefinition extends GwtPage {

    /**
     * Gets the Tab Title
     */
    public String getTitle() {
        return "GnatMetric";
    }

    /**
     * Obtains the Tab Id
     */
    public String getGwtId() {
        return GnatMetricViewer.GWT_ID;
    }
}
