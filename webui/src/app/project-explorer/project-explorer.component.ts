import { Component, OnDestroy, OnInit,
        Input, Output, EventEmitter, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
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
    selector: 'project-explorer',
    templateUrl: './project-explorer.component.html',
    styleUrls: [ 'project-explorer.component.scss' ]
})
export class ProjectExplorerComponent implements OnInit, OnDestroy {
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
        this.reportService.page = 'project-explorer';
    }

    /** @override */
    public ngOnDestroy() {
        this.sub.unsubscribe();
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.report.showed_modules =
            sortMapInArray({newSort: firstSort, otherSort: secondSort},
                           this.reportService.modulesFilter,
                           this.reportService.report.modules);
    }

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

}
