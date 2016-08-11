import { Component } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import { Loader } from '../loader';
import { MapKeys } from '../object.pipe';
import { MissingReportError } from '../errors';

@Component({
    selector: 'report',
    templateUrl: './report.template.html',
    styleUrls: [ './report.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, MissingReportError ],
    pipes: [ MapKeys ],
    providers: [ GNAThubService ]
})
export class Report {
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    ngOnInit(): void {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }
}
