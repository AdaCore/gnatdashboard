/*
 * Sonar Ada Plugin
 *  Copyright (C) 2012-2013, AdaCore
 */
package org.sonar.plugins.ada.ui.gwt.client;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.user.client.ui.HTML;
import org.sonar.gwt.Metrics;
import org.sonar.gwt.ui.Page;
import org.sonar.gwt.ui.ViewerHeader;
import org.sonar.wsclient.services.Measure;
import org.sonar.wsclient.services.Resource;

/**
 * Displays metrics from GnatMetric only, in a UI's source view tab.
 */
public class GnatMetricViewer extends Page {

    //Indicate the module XML file location
    public static final String GWT_ID = "org.sonar.plugins.ada.ui.gwt.GnatMetricViewer";

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
                        // Size
                        Metrics.LINES,
                        Metrics.NCLOC,
                        GwtGnatMetrics.LSLOC,
                        GwtGnatMetrics.BLANK_LINE,

                        // Documentation
                        Metrics.COMMENT_LINES_DENSITY,
                        Metrics.COMMENT_LINES,
                        GwtGnatMetrics.EOL_COMMENTS,

                        // Complexity
                        GwtGnatMetrics.MAX_LOOP_NESTING,
                        GwtGnatMetrics.STATEMENT_COMPLEXITY,
                        GwtGnatMetrics.EXPRESSION_COMPLEXITY,
                        GwtGnatMetrics.ESSENTIAL_COMPLEXITY,
                        Metrics.COMPLEXITY});
        }

        @Override
        protected void display(FlowPanel header, Resource resource) {
            HorizontalPanel panel = new HorizontalPanel();

            addCell(panel,
                    resource.getMeasure(Metrics.LINES),
                    resource.getMeasure(Metrics.NCLOC),
                    resource.getMeasure(GwtGnatMetrics.BLANK_LINE),
                    resource.getMeasure(GwtGnatMetrics.LSLOC));

            addCell(panel,
                    resource.getMeasure(Metrics.COMMENT_LINES_DENSITY),
                    resource.getMeasure(Metrics.COMMENT_LINES),
                    resource.getMeasure(GwtGnatMetrics.EOL_COMMENTS));

            addCell(panel,
                    resource.getMeasure(GwtGnatMetrics.MAX_LOOP_NESTING),
                    resource.getMeasure(Metrics.COMPLEXITY));

            addCell(panel,
                    resource.getMeasure(GwtGnatMetrics.STATEMENT_COMPLEXITY),
                    resource.getMeasure(GwtGnatMetrics.ESSENTIAL_COMPLEXITY));

            addCell(panel,
                    resource.getMeasure(GwtGnatMetrics.EXPRESSION_COMPLEXITY));

            if (panel.getWidgetCount() > 0) {
                header.add(panel);
            }
        }

        @Override
        protected void addCell(Panel panel, Measure... measures) {
            if (measures != null) {
                String names = "";
                String values = "";
                boolean first = true;
                for (Measure measure : measures) {
                    if (measure != null && measure.getFormattedValue() != null) {
                        if (!first) {
                            names += "<br/>";
                            values += "<br/>";
                        }
                        names += "<b>" + measure.getMetricName() + "</b>: ";
                        values += measure.getFormattedValue();
                        first = false;
                    }
                }

                if (!first) {
                    HTML html = new HTML(names);
                    html.setStyleName("metric");
                    panel.add(html);

                    html = new HTML(values);
                    html.setStyleName("value");
                    panel.add(html);
                }
            }
        }
    }
}
