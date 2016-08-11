import { Component } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import { Count } from '../count.pipe';
import { Loader } from '../loader';
import { MissingReportError } from '../missing-report-error';

import '../array-utils';

@Component({
    selector: 'about',
    templateUrl: './about.template.html',
    styleUrls: [ './about.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, MissingReportError ],
    pipes: [ Count ],
    providers: [ GNAThubService ]
})
export class About {
    private report: IGNAThubReport = null;
    private isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    ngOnInit(): void {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    /**
     * @return The total number of sources in the project.
     */
    sourceCount(): number {
        if (!this.report) {
            return 0;
        }
        return Object.keys(this.report.modules)
            .sum(mod => Object.keys(this.report.modules[mod])
                .sum(dir => this.report.modules[mod][dir].length));
    }
}
