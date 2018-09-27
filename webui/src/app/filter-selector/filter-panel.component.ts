import { Component } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { FilterEvent } from '../filter-selector/filter-selector.component';
import {
    IPropertyFilter,
    IRuleFilter,
    IToolFilter,
    IRankingFilter,
    IReviewFilter
} from 'gnat';
import { updateFilter } from '../utils/refreshFilter'
import { sortMessageArray, sortCodeArray } from '../utils/sortArray'

@Component({
    selector: 'filter-panel',
    templateUrl: './filter-panel.component.html',
    styleUrls: [ 'filter-panel.component.scss' ],
})

export class FilterPanelComponent {
    public filterOpen: boolean = true;
    public isReportFetchError: boolean = false;
    public change: number;

    private codeFilter = this.reportService.codeFilter;
    private messageFilter = this.reportService.messageFilter;

    constructor( public reportService: SharedReport) {}

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

    private refreshSorting(){
        let codeFilter = this.reportService.codeFilter;
        let messageFilter = this.reportService.messageFilter;
        sortMessageArray(this.messageFilter, this.messageFilter, this.reportService.message.sources);
        sortCodeArray(this.codeFilter, this.codeFilter, this.reportService.code.modules)
    }

    /*
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent) {
        const tool = <IToolFilter> event.option;
        this.reportService.filter.tools[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent) {
        const rule = <IRuleFilter> event.option;
        this.reportService.filter.rules[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent) {
        const property = <IPropertyFilter> event.option;
        this.reportService.filter.properties[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the ranking filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRankingFilterToggle(event: FilterEvent) {
        const rank = <IRankingFilter> event.option;
        this.reportService.filter.ranking[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the ranking filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onReviewFilterToggle(event: FilterEvent) {
        const review = <IReviewFilter> event.option;
        this.reportService.filter.review_status[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

}
