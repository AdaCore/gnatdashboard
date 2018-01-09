import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import { GNAThubService } from '../gnathub.service';

import { FilterEvent } from '../filter-selector/filter-selector.component';
import { sortMapInArray, sortBy } from './project-sort.component';
import {
    IPropertyFilter,
    IReportIndex,
    IRuleFilter,
    IToolFilter
} from 'gnat';

@Component({
    selector: 'project-explorer',
    templateUrl: './project-explorer.component.html',
    styleUrls: [ 'project-explorer.component.scss' ]
})
export class ProjectExplorerComponent implements OnInit, OnDestroy {
    public project: string;
    public directory: string;
    public change: number;
    public report: IReportIndex;
    public modulesFilter = {newSort: 'name', otherSort: '', order: -1};
    public isReportFetchError: boolean = false;
    private sub: Subscription;

    constructor( private gnathub: GNAThubService, private route: ActivatedRoute) {}
    /** @override */
    public ngOnInit() {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
            this.change = 0;
        });
        this.gnathub.getReport().subscribe(
            report => {
                this.report = report;
                this.report.showed_modules =
                    sortMapInArray({newSort: 'name', otherSort: ''},
                                   this.modulesFilter, this.report.modules);
            }, error => this.isReportFetchError = !!error
        );
    }

    /** @override */
    public ngOnDestroy() {
        this.sub.unsubscribe();
    }

    /*
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent) {
        const tool = <IToolFilter> event.option;
        this.report.tools[tool.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent) {
        const rule = <IRuleFilter> event.option;
        this.report.rules[rule.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent) {
        const property = <IPropertyFilter> event.option;
        this.report.properties[property.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Call the function to sort, sending the rights arguments
     */

    public sortModules(firstSort: string, secondSort: string) {
        this.report.showed_modules =
            sortMapInArray({newSort: firstSort, otherSort: secondSort},
                           this.modulesFilter, this.report.modules);
    }

    private updateMessagesUiProperties() {
        const tools = this.report.tools;
        const rules = this.report.rules;
        const properties = this.report.properties;

        Object.keys(tools).forEach(id =>
                                   tools[id]._ui_selected_message_count = 0);
        Object.keys(rules).forEach(id =>
                                   rules[id]._ui_selected_message_count = 0);
        Object.keys(properties).forEach(id =>
                                        properties[id]._ui_selected_message_count = 0);

        Object.keys(this.report.modules).forEach(name => {
            const myModule = this.report.modules[name];
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
        this.report.showed_modules = sortBy(this.modulesFilter,
                                            this.modulesFilter, this.report.modules);
        this.change++;
    }
}
