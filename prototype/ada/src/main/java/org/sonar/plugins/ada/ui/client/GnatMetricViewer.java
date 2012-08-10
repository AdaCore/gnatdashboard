/*
 * Sonar Ada Plugin
 * Copyright (C) 2012, AdaCore
 */
package org.sonar.plugins.ada.ui.client;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Widget;
import org.sonar.gwt.Metrics;
import org.sonar.gwt.ui.Page;
import org.sonar.gwt.ui.ViewerHeader;
import org.sonar.wsclient.services.Resource;

/**
 * Displays metrics from GnatMetric only, in a
 * UI's source view tab.
 */
public class GnatMetricViewer extends Page {

    //Indicate the module XML file location
    public static final String GWT_ID = "org.sonar.plugins.ada.ui.GnatMetricViewer";

    @Override
    protected final Widget doOnResourceLoad(Resource resource) {
        FlowPanel panel = new FlowPanel();
        panel.setWidth("100%");
        panel.add(new GnatMetricHeader(resource));
        return panel;
    }

    private static class GnatMetricHeader extends ViewerHeader {

        public GnatMetricHeader(Resource resource) {
            super(resource, new String[]{
                        Metrics.LINES,
                        Metrics.NCLOC,
                        GwtGnatMetrics.BLANK_LINE,

                        Metrics.COMMENT_LINES_DENSITY,
                        Metrics.COMMENT_LINES,
                        GwtGnatMetrics.EOL_COMMENTS});
        }

        @Override
        protected void display(FlowPanel header, Resource resource) {
            HorizontalPanel panel = new HorizontalPanel();

            addCell(panel,
                    resource.getMeasure(Metrics.LINES),
                    resource.getMeasure(Metrics.NCLOC),
                    resource.getMeasure(GwtGnatMetrics.BLANK_LINE));

            addCell(panel,
                    resource.getMeasure(Metrics.COMMENT_LINES_DENSITY),
                    resource.getMeasure(Metrics.COMMENT_LINES),
                    resource.getMeasure(GwtGnatMetrics.EOL_COMMENTS));

            if (panel.getWidgetCount() > 0) {
                header.add(panel);
            }
        }
    }
}
