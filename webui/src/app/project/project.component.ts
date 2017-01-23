import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { Subscription } from 'rxjs/Subscription';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport, ISource } from 'gnat';

import '../array/operator/sum';

@Component({
    selector: 'project',
    templateUrl: './project.component.html',
    styleUrls: [ 'project.component.scss' ],
})
export class ProjectComponent implements OnDestroy, OnInit {
    public project: string = null;
    public directory: string = null;
    public report: IGNAThubReport = null;
    public isReportFetchError: boolean = false;
    private sub: Subscription = null;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute) {}

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

    public getMessageCount(source: ISource): number {
        if (!source.message_count) {
            return 0;
        }
        return Object.keys(source.message_count).sum(
            toolId => source.message_count[toolId]);
    }
}
