import { Component, OnDestroy, OnInit,
        Input, Output, EventEmitter, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { SharedReport } from '../main-responder.service';

import { FilterEvent } from '../filter-selector/filter-selector.component';
import { sortCodeArray } from './project-sort.component';
import {
    IPropertyFilter,
    IReportIndex,
    IRuleFilter,
    IToolFilter
} from 'gnat';
@Component({
    selector: 'code-explorer',
    templateUrl: './code-explorer.component.html',
    styleUrls: [ 'code-explorer.component.scss' ]
})
export class CodeExplorerComponent implements OnInit, OnDestroy {
    public project: string;
    public directory: string;
    private sub: Subscription;

    constructor(private route: ActivatedRoute,
                 private reportService: SharedReport,
                @Inject(DOCUMENT) private document: Document) {}
    /** @override */
    public ngOnInit() {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
/*            if (this.project) { this.openClose(this.project); }
            if (this.project) { this.openClose(this.directory); }*/
        });
        this.reportService.page = 'code-explorer';
    }

    /** @override */
    public ngOnDestroy() {
        this.sub.unsubscribe();
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.code.modules =
            sortCodeArray({newSort: firstSort, otherSort: secondSort},
                           this.reportService.codeFilter,
                           this.reportService.code.modules);
    }

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

}
