import { Component } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import '../array-utils';

@Component({
    selector: 'about',
    templateUrl: './about.template.html',
    styleUrls: [ './about.style.css' ],
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
