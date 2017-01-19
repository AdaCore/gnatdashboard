import { Component, OnInit } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubReport } from 'gnat';

import '../array/operator/sum';

@Component({
    selector: 'about',
    templateUrl: './about.component.html',
    styleUrls: [ 'about.component.scss' ],
    providers: [ GNAThubService ]
})
export class AboutComponent implements OnInit {
    public report: IGNAThubReport = null;
    public isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    public ngOnInit(): void {
        this.gnathub.getReport().subscribe(
            report => this.report = report,
            error => this.isReportFetchError = !!error);
    }

    /**
     * @return The total number of sources in the project.
     */
    public sourceCount(): number {
        if (!this.report) {
            return 0;
        }
        return Object.keys(this.report.modules)
            .sum(mod => Object.keys(this.report.modules[mod].source_dirs)
                .sum(dir =>
                    this.report.modules[mod].source_dirs[dir].sources.length));
    }
}
