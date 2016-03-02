import { Component } from "angular2/core";
import { CORE_DIRECTIVES } from "angular2/common";
import {
    CanReuse, ComponentInstruction, OnReuse, RouteParams, RouterLink
} from "angular2/router";

import { IGNAThubReport } from "gnat/reports";

import { MapKeys } from "../../pipes/object";
import { ReportService } from "../../services/report";

@Component({
    selector: "gnathub-report",
    templateUrl: "app/components/gnathub-report/gnathub-report.html",
    directives: [ CORE_DIRECTIVES, RouterLink ],
    pipes: [ MapKeys ],
    providers: [ ReportService ]
})
export class GNAThubReport implements CanReuse, OnReuse {
    private report: IGNAThubReport = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
     */
    constructor(private reportService: ReportService, route: RouteParams) {
        this.readRouteParameters(route.params);
    }

    /**
     * Query the annotated source data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.reportService.GNAThubReport(
            (report: IGNAThubReport) => this.report = report);
    }

    /** @override */
    public routerCanReuse(
            next: ComponentInstruction, prev: ComponentInstruction): boolean
    {
        // Always allow reuse since this component fetches the entire list of
        // traces.
        return true;
    }

    /** @override */
    public routerOnReuse(
            next: ComponentInstruction, prev: ComponentInstruction): void
    {
        // Refresh the |program| attribute because the URL was likely updated.
        this.readRouteParameters(next.params);
    }

    /**
     * Update attributes wrt. route parameters.
     *
     * @param params An immutable map of parameters.
     */
    private readRouteParameters(params: { [key: string]: string }): void {
        // Nothing to do
    }
}
