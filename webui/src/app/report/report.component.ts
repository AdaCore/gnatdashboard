import { Component, OnInit } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

@Component({
    selector: 'report',
    templateUrl: './report.component.html',
    styleUrls: [ 'report.component.scss' ]
})
export class ReportComponent implements OnInit {
    public report: IGNAThubReport = null;
    public isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    public ngOnInit(): void {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }
}
