import { Component } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { FilterEvent } from '../filter-selector/filter-selector.component';
import {
    IPropertyFilter,
    IRuleFilter,
    IToolFilter,
    IRankingFilter
} from 'gnat';

@Component({
    selector: 'filter-panel',
    templateUrl: './filter-panel.component.html',
    styleUrls: [ 'filter-panel.component.scss' ],
})

export class FilterPanelComponent {
    public filterOpen: boolean = true;
    public isReportFetchError: boolean = false;
    public change: number;

    constructor( public reportService: SharedReport) {
    }

    public openCloseFilterTab() {
        let button = document.getElementById('chrevonButton');
        button.classList.toggle('reduce');
        button.classList.toggle('open');
        let filter = document.getElementById('filterBar');
        filter.classList.toggle('reduce');
        filter.classList.toggle('open');
        let panel = document.getElementById('filterPanel');
        panel.classList.toggle('reduce');
        panel.classList.toggle('open');
        this.filterOpen = !this.filterOpen;
    }

    public hideFilesChanges() {
        this.reportService.hideFiles = !this.reportService.hideFiles;
    }

    /*
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent) {
        const tool = <IToolFilter> event.option;
        this.reportService.filter.tools[event.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent) {
        const rule = <IRuleFilter> event.option;
        this.reportService.filter.rules[event.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent) {
        const property = <IPropertyFilter> event.option;
        this.reportService.filter.properties[event.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /*
     * Checkbox handled for the ranking filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRankingFilterToggle(event: FilterEvent) {
        const rank = <IRankingFilter> event.option;
        this.reportService.filter.ranking[event.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    private isSelected(id: number, array: any): boolean {
        let isSelected: boolean;

        array.forEach(function(cell){
            if (cell.id === id){
                if (cell._ui_unselected) {
                    isSelected = !cell._ui_unselected;
                } else {
                    isSelected = true;
                }
            }
        });
        return isSelected;
    }

    private incMessageCount(id: number, array: any){
        array.forEach(function(cell){
            if (cell.id === id){
                cell._ui_selected_message_count += 1;
            }
        });
    }

    private updateMessagesUiProperties() {
        const tools = this.reportService.filter.tools;
        const rules = this.reportService.filter.rules;
        const properties = this.reportService.filter.properties;
        const ranking = this.reportService.filter.ranking;
        this.reportService._ui_total_message_count = 0;

        tools.forEach(function(tool){
            tool._ui_selected_message_count = 0;
        });
        rules.forEach(function(rule){
            rule._ui_selected_message_count = 0;
        });
        properties.forEach(function(property){
            property._ui_selected_message_count = 0;
        });
        ranking.forEach(function(rank){
            rank._ui_selected_message_count = 0;
        });
        this.reportService.message.sources.forEach(function(source){
            source._ui_total_message_count = 0;
        });

        this.reportService.code.modules.forEach(function(myModule){
            myModule._ui_total_message_count = 0;

            myModule.source_dirs.forEach(function(folder){
                folder._ui_total_message_count = 0;

                folder.sources.forEach(function(codeSource){
                    codeSource._ui_total_message_count = 0;

                    this.reportService.message.sources.forEach(function(source){
                        if (source.filename === codeSource.filename &&
                            source.messages != null){
                            source.messages.forEach(function(message){
                                const toolId = message.rule.tool_id;
                                const ruleId = message.rule.id;
                                const rankId = message.ranking.id;
                                const isToolSelected = this.isSelected(toolId, tools);
                                const isRuleSelected = this.isSelected(ruleId, rules);
                                const isRankSelected = this.isSelected(rankId, ranking)
                                let hasSelectedProperties = false;

                                if (message.properties != null){
                                    if (message.properties.length === 0){
                                        hasSelectedProperties = true;
                                    }
                                    message.properties.forEach(function(property){
                                        let isPropertySelected =
                                            this.isSelected(property.id, properties);
                                        if (isPropertySelected){
                                            hasSelectedProperties = true;
                                        }
                                    }.bind(this));
                                }

                                if (isToolSelected && isRuleSelected && isRankSelected && hasSelectedProperties) {
                                    this.incMessageCount(toolId, tools);
                                    this.incMessageCount(ruleId, rules);
                                    this.incMessageCount(rankId, ranking);
                                    message.properties.forEach(function(property){
                                        let isPropertySelected =
                                            this.isSelected(property.id, properties);
                                        if (isPropertySelected){
                                            this.incMessageCount(property.id, properties);
                                        }
                                    }.bind(this));

                                    message.hide = true;
                                    source._ui_total_message_count ++;
                                    codeSource._ui_total_message_count ++;
                                    folder._ui_total_message_count++;
                                    myModule._ui_total_message_count++;
                                    this.reportService._ui_total_message_count ++;
                                } else {
                                    message.hide = false;
                                }
                            }.bind(this));
                        }
                    }.bind(this));
                }.bind(this));
            }.bind(this));
        }.bind(this));
        this.change++;
    }
}