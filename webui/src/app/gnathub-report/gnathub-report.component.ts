import { Component, ViewEncapsulation } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES } from '@angular/router';

import { IGNAThubReport } from 'gnat';

import { Loader } from '../loader/loader.component';
import { MapKeys } from '../object.pipe';
import { ReportService } from '../report.service';

@Component({
    selector: 'gnathub-report',
    templateUrl: './gnathub-report.template.html',
    styleUrls: [ './gnathub-report.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, ROUTER_DIRECTIVES ],
    pipes: [ MapKeys ],
    providers: [ ReportService ]
})
export class GNAThubReport {
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
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
