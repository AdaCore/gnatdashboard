import { Component, OnInit } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IReportIndex } from 'gnat';

@Component({
    selector: 'source-list',
    templateUrl: './source-list.component.html',
    styleUrls: [ 'source-list.component.scss' ]
})
export class SourceListComponent implements OnInit {
    public report: IReportIndex;
    public isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    /** @override */
    public ngOnInit() {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }
}
