import { Component } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { IGNAThubReport } from 'gnat';
import { GNAThubService } from '../gnathub.service';

import { Subscription } from 'rxjs';
import { IGNAThubProperty, IGNAThubRule, IGNAThubTool } from 'gnat';

@Component({
    selector: 'project-explorer',
    templateUrl: './project-explorer.component.html',
    styleUrls: [ 'project-explorer.component.scss' ]
})
export class ProjectExplorerComponent {
    public project: string = null;
    public directory: string = null;
    public report: IGNAThubReport = null;
    public isReportFetchError: boolean = false;
    private sub: Subscription = null;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute) {}

    public ngOnInit(): void {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
        });
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    public ngOnDestroy(): void {
        this.sub.unsubscribe();
    }

    /**
     * @param tool The tool which messages this function counts.
     * @return The total number of messages displayed for a given tool.
     */
    public toolMessageCount = (tool: IGNAThubTool): number => {
        return 0;
    }

    /**
     * @param rule The rule which messages this function counts.
     * @return The total number of messages displayed for a given rule.
     */
    public ruleMessageCount = (rule: IGNAThubRule): number => {
        return 0;
    }

    /**
     * @param property The property which messages this function counts.
     * @return The total number of messages displayed for a given property.
     */
    public propertyMessageCount = (property: IGNAThubProperty): number => {
        return 0;
    }
}
