import { Component } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { FilterEvent } from '../filter-selector/filter-selector.component';
import {
    IPropertyFilter,
    IRuleFilter,
    IToolFilter,
    IRankingFilter,
    IReviewFilter,
    ISort
} from 'gnat';
import { updateFilter } from '../utils/refreshFilter';
import { sortMessageArray, sortCodeArray } from '../utils/sortArray';

@Component({
    selector: 'filter-panel',
    templateUrl: './filter-panel.component.html',
    styleUrls: [ 'filter-panel.component.scss' ],
})

export class FilterPanelComponent {
    public filterOpen: boolean = true;
    public isReportFetchError: boolean = false;
    public change: number;

    private projectSort: ISort = this.reportService.projectSort;
    private messageSort: ISort = this.reportService.messageSort;

    constructor( public reportService: SharedReport) {}

    public openCloseFilterTab(): void {
        let button: HTMLElement = document.getElementById('chrevonButton');
        button.classList.toggle('reduce');
        button.classList.toggle('open');
        let filter: HTMLElement = document.getElementById('filterBar');
        filter.classList.toggle('reduce');
        filter.classList.toggle('open');
        let panel: HTMLElement = document.getElementById('filterPanel');
        panel.classList.toggle('reduce');
        panel.classList.toggle('open');
        this.filterOpen = !this.filterOpen;
    }

    private refreshSorting(): void {
        let projectSort: ISort = this.reportService.projectSort;
        let messageSort: ISort = this.reportService.messageSort;
        sortMessageArray(this.messageSort, this.messageSort, this.reportService.message.sources);
        sortCodeArray(this.projectSort, this.projectSort, this.reportService.code.modules);
    }

    /*
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent): void {
        const tool: IToolFilter = <IToolFilter> event.option;
        this.reportService.filter.tools[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent): void {
        const rule: IRuleFilter = <IRuleFilter> event.option;
        this.reportService.filter.rules[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent): void {
        const property: IPropertyFilter = <IPropertyFilter> event.option;
        this.reportService.filter.properties[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the ranking filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRankingFilterToggle(event: FilterEvent): void {
        const rank: IRankingFilter = <IRankingFilter> event.option;
        this.reportService.filter.ranking[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

    /*
     * Checkbox handled for the ranking filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onReviewFilterToggle(event: FilterEvent): void {
        const review: IReviewFilter = <IReviewFilter> event.option;
        this.reportService.filter.review_status[event.id]._ui_unselected = !event.checked;
        updateFilter(this.reportService);
        this.refreshSorting();
    }

}
