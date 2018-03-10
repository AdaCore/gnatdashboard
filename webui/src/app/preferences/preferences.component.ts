import { Component, OnInit } from '@angular/core';

import { GNAThubService } from '../gnathub.service';
import { IReportIndex } from 'gnat';

import '../array/operator/sum';

@Component({
    selector: 'preferences',
    templateUrl: './preferences.component.html',
    styleUrls: [ 'preferences.component.scss' ]
})
export class PreferencesComponent implements OnInit {
    public report: IReportIndex;
    public isReportFetchError: boolean = false;

    constructor(private gnathub: GNAThubService) {}

    /** @override */
    public ngOnInit() {
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
                .sum(dir => Object.keys(
                    this.report.modules[mod].source_dirs[dir].sources).length));
    }
}
