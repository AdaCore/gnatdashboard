import { Component, ViewEncapsulation } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES, Router } from '@angular/router';

import { Subscription } from 'rxjs/Subscription';

import { IGNATcoverageReport } from 'gnat';

import { PathEncoder } from '../path-encoder';
import { ReportService } from '../report.service';

interface IProgramTrace {
    filename: string;
    gen_date: Date;
    tag: string;
}

@Component({
    selector: 'trace-list',
    encapsulation: ViewEncapsulation.None,
    templateUrl: './trace-list.template.html',
    styleUrls: [ './trace-list.style.css' ],
    directives: [ CORE_DIRECTIVES, ROUTER_DIRECTIVES ],
    providers: [ ReportService ]
})
export class TraceList extends PathEncoder {
    private program: string = null;
    private programs: string[] = null;
    private traces: { [program: string]: IProgramTrace[] } = null;
    private report: IGNATcoverageReport = null;
    private sub: Subscription = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The
     */
    constructor(private reportService: ReportService, private router: Router) {
        super();
    }

    /**
     * Query the report data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.sub = this.router
            .routerState
            .queryParams
            .subscribe(params => {
                this.program = params.hasOwnProperty('program') ?
                    this.decodePath(params['program']) : null;
            });

        this.reportService.GNATcovReport((report: IGNATcoverageReport) => {
            const traces: { [program: string]: IProgramTrace[] } = {};

            for (const trace of report.traces) {
                const program: string = trace['program'];
                const record: IProgramTrace = {
                    filename: trace['filename'],
                    gen_date: new Date(trace['date']),
                    tag: trace['tag']
                };

                if (traces.hasOwnProperty(program)) {
                    traces[program].push(record);
                } else {
                    traces[program] = [record];
                }
            }

            this.programs = Object.keys(traces);
            this.traces = traces;
            this.report = report;
        });
    }

    /** @override */
    ngOnDestroy() {
        this.sub.unsubscribe();
    }

    /**
     * Compute the total number of loaded traces.
     *
     * @return The trace count.
     */
    public getTraceFileCount(): number {
        if (this.report) {
            return this.report.traces.length;
        }
        return 0;
    }

    /**
     * Compute the total number of programs run.
     *
     * @return The program count.
     */
    public getProgramCount(): number {
        if (this.programs) {
            return this.programs.length;
        }
        return 0;
    }
}
