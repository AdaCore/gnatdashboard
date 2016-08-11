import { Component } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES } from '@angular/router';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import { Loader } from '../loader';
import { MapKeys } from '../object.pipe';
import { MissingReportError } from '../missing-report-error';

@Component({
    selector: 'source-list',
    templateUrl: './source-list.template.html',
    styleUrls: [ './source-list.style.css' ],
    directives: [
        CORE_DIRECTIVES, Loader, MissingReportError, ROUTER_DIRECTIVES
    ],
    pipes: [ MapKeys ],
    providers: [ GNAThubService ]
})
export class SourceList {
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    ngOnInit(): void {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }
}
