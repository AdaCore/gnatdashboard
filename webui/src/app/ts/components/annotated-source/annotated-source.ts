import { Component } from "angular2/core";
import { CORE_DIRECTIVES } from "angular2/common";
import { RouteParams, RouterLink } from "angular2/router";

import { IGNATcoverageHunk } from "gnat/reports";

import { highlightAda } from "../../ada-lang";
import { unescapeHTML } from "../../html-utils";
import { PathEncoder } from "../../path-encoder";
import { ReportService } from "../../services/report";

@Component({
    selector: "annotated-source",
    templateUrl: "app/components/annotated-source/annotated-source.html",
    directives: [ CORE_DIRECTIVES, RouterLink ],
    providers: [ ReportService ]
})
export class AnnotatedSource extends PathEncoder {
    private filename: string = null;
    private source: IGNATcoverageHunk = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
     */
    constructor(private reportService: ReportService, route: RouteParams) {
        super();
        this.readRouteParameters(route.params);
    }

    /**
     * Query the annotated source data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.reportService.AnnotatedSource((hunk: IGNATcoverageHunk) => {
            this.source = hunk;
        });
    }

    /**
     * Syntax highlight a code snippet.
     *
     * @param code The source code to highlight.
     * @return The HTML string with highlighting markup.
     */
    public highlight(code: string): string {
        // TODO(delay): avoid having to deal with encoded HTML entities at this
        // point (ie. remove the call to |unescapeHTML|).
        return highlightAda(unescapeHTML(code));
    }

    /**
     * Update attributes wrt. route parameters.
     *
     * @param params An immutable map of parameters.
     */
    private readRouteParameters(params: { [key: string]: string }): void {
        this.filename = params.hasOwnProperty("filename") ?
            this.decodePath(params["filename"]) : null;
    }
}
