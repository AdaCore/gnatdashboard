import { Component, OnDestroy, OnInit,
        Input, Output, EventEmitter, Inject } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { SharedReport } from '../main-responder.service';

import { FilterEvent } from '../filter-selector/filter-selector.component';
import { sortMessageArray } from '../utils/sortArray';
import {
    IPropertyFilter,
    IReportIndex,
    IRuleFilter,
    IToolFilter
} from 'gnat';

import { DOCUMENT } from '@angular/platform-browser';

@Component({
    selector: 'message-explorer',
    templateUrl: './message-explorer.component.html',
    styleUrls: [ 'message-explorer.component.scss' ]
})
export class MessageExplorerComponent implements OnInit {

    constructor(private route: ActivatedRoute,
                public reportService: SharedReport,
                @Inject(DOCUMENT) private document: Document) {}
    /** @override */
    public ngOnInit() {
        this.reportService.page = 'message-explorer';
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.message.sources = sortMessageArray(
            {newSort: firstSort, otherSort: secondSort},
            this.reportService.messageFilter,
            this.reportService.message.sources);
    }

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public expandCollapseAll(badClass: string) {
        this.reportService.message.sources.forEach(function(source){
            let elem = this.document.getElementById(source.filename);
            if (elem) {
                let idx = elem.classList.contains(badClass);
                if (idx) {
                    this.openClose(source.filename);
                }
            }
        }.bind(this));
    }

}
