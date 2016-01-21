import { Component } from "angular2/core";
import { CORE_DIRECTIVES } from "angular2/common";
import {
    CanReuse, ComponentInstruction, OnReuse, RouteParams, RouterLink
} from "angular2/router";

import { IGNATcoverageReport } from "../../model/gnatcoverage-report";
import { ReportService } from "../../services/report";

interface IProgramTrace {
    filename: string;
    gen_date: Date;
    tag: string;
}

@Component({
    selector: "trace-list",
    templateUrl: "app/components/trace-list/trace-list.html",
    directives: [ CORE_DIRECTIVES, RouterLink ],
    providers: [ ReportService ]
})
export class TraceList implements CanReuse, OnReuse {
    private program: string = null;
    private programs: string[] = null;
    private traces: { [program: string]: IProgramTrace[] } = null;
    private report: IGNATcoverageReport = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The
     */
    constructor(private reportService: ReportService, route: RouteParams) {
        this.readRouteParameters(route.params);
    }

    /**
     * Injected in the template context.
     *
     * @param path The program path to encode to use as an URL parameter.
     * @return The encoded program path.
     */
    encodePath(path: string): string {
        return encodeURIComponent(path);
    }

    /**
     * For consistency with |encodePath|.
     *
     * @param path The program path to decode to use as an URL parameter.
     * @return The decoded program path.
     */
    decodePath(path: string): string {
        return decodeURIComponent(path);
    }

    /** @override */
    routerCanReuse(next: ComponentInstruction, prev: ComponentInstruction) {
        // Always allow reuse since this component fetches the entire list of
        // traces.
        return true;
    }

    /** @override */
    routerOnReuse(next: ComponentInstruction, prev: ComponentInstruction) {
        // Refresh the |program| attribute because the URL was likely updated.
        this.readRouteParameters(next.params);
    }

    /**
     * Update attributes wrt. route parameters.
     *
     * @param params An immutable map of parameters.
     */
    private readRouteParameters(params: { [key: string]: any }): void {
        this.program = params.hasOwnProperty("program") ?
            this.decodePath(params["program"]) : null;
    }

    /**
     * Query the report data and store a reference to it.
     *
     * @override
     */
    private ngOnInit(): void {
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

    /**
     * Compute the total number of loaded traces.
     *
     * @return The trace count.
     */
    private getTraceFileCount(): number {
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
    private getProgramCount(): number {
        if (this.programs) {
            return this.programs.length;
        }
        return 0;
    }
}
