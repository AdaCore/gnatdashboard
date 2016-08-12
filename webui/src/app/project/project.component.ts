import { Component } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ActivatedRoute } from '@angular/router';

import { Subscription } from 'rxjs/Subscription';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import { Count } from '../count.pipe';
import { Loader } from '../loader';
import { MapKeys } from '../object.pipe';
import { MissingReportError } from '../errors';

@Component({
    selector: 'project',
    templateUrl: './project.template.html',
    styleUrls: [ './project.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, MissingReportError ],
    pipes: [ Count, MapKeys ],
    providers: [ GNAThubService ]
})
export class Project {
    private project: string = null;
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;
    private sub: Subscription = null;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute) {}

    ngOnInit(): void {
        this.sub = this.route.params.subscribe(params => {
            this.project = params['name'];
        });
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    ngOnDestroy(): void {
        this.sub.unsubscribe();
    }
}
