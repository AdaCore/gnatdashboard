import { Component, OnInit } from '@angular/core';

import { GNAThubService } from '../gnathub.service';


@Component({
    selector: 'source-list',
    templateUrl: './source-list.component.html',
    styleUrls: [ 'source-list.component.scss' ]
})
export class SourceListComponent implements OnInit {
    public isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    /** @override */
    public ngOnInit() {

    }
}
