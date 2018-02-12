import { Component, OnDestroy, OnInit,
        Input, Output, EventEmitter } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { SharedReport } from '../main-responder.service';

import { FilterEvent } from '../filter-selector/filter-selector.component';
import { sortMapInArray } from './project-sort.component';
import {
    IPropertyFilter,
    IReportIndex,
    IRuleFilter,
    IToolFilter
} from 'gnat';
@Component({
    selector: 'error-explorer',
    templateUrl: './error-explorer.component.html',
    styleUrls: [ 'error-explorer.component.scss' ]
})
export class ErrorExplorerComponent implements OnInit {

    constructor(private route: ActivatedRoute,
                 private reportService: SharedReport) {}
    /** @override */
    public ngOnInit() {
        console.log("This part is in progress.");
        this.reportService.page = 'error-explorer';
    }


}
