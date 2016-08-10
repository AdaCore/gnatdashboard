import { Component, ViewEncapsulation } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES } from '@angular/router';

import { IGNAThubReport } from 'gnat';

import { Loader } from '../loader';
import { MapKeys } from '../object.pipe';
import { GNAThubService } from '../gnathub.service';

@Component({
    selector: 'source-list',
    templateUrl: './source-list.template.html',
    styleUrls: [ './source-list.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, ROUTER_DIRECTIVES ],
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
