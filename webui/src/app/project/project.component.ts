import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { Subscription } from 'rxjs/Subscription';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

@Component({
    selector: 'project',
    templateUrl: './project.component.html',
    styleUrls: [ 'project.component.scss' ],
    providers: [ GNAThubService ]
})
export class ProjectComponent implements OnDestroy, OnInit {
    public project: string = null;
    public directory: string = null;
    public report: IGNAThubReport = null;
    public isReportFetchError: boolean = false;
    private sub: Subscription = null;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute,
        private router: Router) {}

    public ngOnInit(): void {
        this.project = this.route.snapshot.params['name'];
        this.sub = this.route.queryParams.subscribe(params => {
            this.directory = params['directory'];
        });
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    public ngOnDestroy(): void {
        this.sub.unsubscribe();
    }
}
