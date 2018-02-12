import { Component, Input } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { FilterEvent } from '../filter-selector/filter-selector.component';
import { sortMapInArray, sortBy } from '../project-explorer/project-sort.component';
import {
    IPropertyFilter,
    IReportIndex,
    IRuleFilter,
    IToolFilter
} from 'gnat';

@Component({
    selector: 'filter-panel',
    templateUrl: './filter-panel.component.html',
    styleUrls: [ 'filter-panel.component.scss' ],
})

export class FilterPanelComponent {
    public filter_open: boolean = true;
    public isReportFetchError: boolean = false;
    public change: number;

    constructor( private reportService: SharedReport) {}

    public openCloseFilterTab() {
        let button = document.getElementById("chrevonButton")
        button.classList.toggle("reduce");
        button.classList.toggle("open")
        let filter = document.getElementById("filterBar")
        filter.classList.toggle("reduce");
        filter.classList.toggle("open")
        let panel = document.getElementById("filterPanel")
        panel.classList.toggle("reduce");
        panel.classList.toggle("open")
        this.filter_open = !this.filter_open;
    }

    /*
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent) {
        const tool = <IToolFilter> event.option;
        this.reportService.report.tools[tool.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent) {
        const rule = <IRuleFilter> event.option;
        this.reportService.report.rules[rule.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent) {
        const property = <IPropertyFilter> event.option;
        this.reportService.report.properties[property.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    private updateMessagesUiProperties() {
        const tools = this.reportService.report.tools;
        const rules = this.reportService.report.rules;
        const properties = this.reportService.report.properties;

        Object.keys(tools).forEach(
            id => tools[id]._ui_selected_message_count = 0);
        Object.keys(rules).forEach(
            id => rules[id]._ui_selected_message_count = 0);
        Object.keys(properties).forEach(
            id => properties[id]._ui_selected_message_count = 0);

        Object.keys(this.reportService.report.modules).forEach(name => {
            const myModule = this.reportService.report.modules[name];
            myModule._ui_total_message_count = 0;
            Object.keys(myModule.source_dirs).forEach(path => {
                const sourceDir = myModule.source_dirs[path];
                sourceDir._ui_total_message_count = 0;
                Object.keys(sourceDir.sources).forEach(filename => {
                    const source = sourceDir.sources[filename];
                    source._ui_total_message_count = 0;
                    if (!source._messages) {
                        return;
                    }
                    source._messages.forEach(message => {
                        const tid = message.tool_id;
                        const rid = message.rule_id;
                        const isToolSelected = !tools[tid]._ui_unselected;
                        const isRuleSelected = !rules[rid]._ui_unselected;
                        const hasSelectedProperties = !message.property_ids
                        || !message.property_ids.length
                        || message.property_ids.some(id => !properties[id]._ui_unselected);
                        if (isToolSelected && isRuleSelected && hasSelectedProperties) {
                            tools[tid]._ui_selected_message_count++;
                            rules[rid]._ui_selected_message_count++;
                            if (message.property_ids) {
                                message.property_ids.forEach(id => {
                                    const property = properties[id];
                                    property._ui_selected_message_count++;
                                });
                            }
                            source._ui_total_message_count++;
                            sourceDir._ui_total_message_count++;
                            myModule._ui_total_message_count++;
                        }
                    });
                });
            });
        });
        this.reportService.report.showed_modules =
            sortBy(this.reportService.modulesFilter,
                   this.reportService.modulesFilter,
                   this.reportService.report.modules);
        this.change++;
    }

}


