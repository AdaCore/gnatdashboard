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

    constructor(private reportService: ReportService) {}

    ngOnInit(): void {
        this.reportService.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }
}
