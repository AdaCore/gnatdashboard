import { Component, ViewEncapsulation } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES } from '@angular/router';

import { IGNAThubReport } from 'gnat';

import { Loader } from '../loader/loader.component';
import { MapKeys } from '../object.pipe';
import { ReportService } from '../report.service';

@Component({
    selector: 'source-list',
    templateUrl: './source-list.template.html',
    styleUrls: [ './source-list.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, ROUTER_DIRECTIVES ],
    pipes: [ MapKeys ],
    providers: [ ReportService ]
})
export class SourceList {
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;

    /**
     * @param reportService Custom service to retrieve reports data.
     */
    constructor(private reportService: ReportService) { }

    /**
     * Query the annotated source data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.reportService.GNAThubReport((report: IGNAThubReport) => {
            this.report = report;
            this.isReportFetchError = report === null;
        });
    }
}
