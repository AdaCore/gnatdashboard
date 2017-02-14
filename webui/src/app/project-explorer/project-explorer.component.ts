import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { IReportIndex } from 'gnat';
import { GNAThubService } from '../gnathub.service';

import { Subscription } from 'rxjs';
import { IProperty, IRule, ITool } from 'gnat';

@Component({
    selector: 'project-explorer',
    templateUrl: './project-explorer.component.html',
    styleUrls: [ 'project-explorer.component.scss' ]
})
export class ProjectExplorerComponent implements OnInit, OnDestroy {
    public project: string;
    public directory: string;
    public report: IReportIndex;
    public isReportFetchError: boolean = false;
    private sub: Subscription;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute) {}

    /** @override */
    public ngOnInit() {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
        });
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    /** @override */
    public ngOnDestroy() {
        this.sub.unsubscribe();
    }

    /**
     * @param tool The tool which messages this function counts.
     * @return The total number of messages displayed for a given tool.
     */
    public toolMessageCount = (tool: ITool): number => {
        return 0;
    }

    /**
     * @param rule The rule which messages this function counts.
     * @return The total number of messages displayed for a given rule.
     */
    public ruleMessageCount = (rule: IRule): number => {
        return 0;
    }

    /**
     * @param property The property which messages this function counts.
     * @return The total number of messages displayed for a given property.
     */
    public propertyMessageCount = (property: IProperty): number => {
        return 0;
    }
}
