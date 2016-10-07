import { Component } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

@Component({
    selector: 'source-list',
    templateUrl: './source-list.template.html',
    styleUrls: [ './source-list.style.css' ],
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