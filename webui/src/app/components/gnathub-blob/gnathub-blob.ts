import { Component } from "angular2/core";
import { CORE_DIRECTIVES } from "angular2/common";
import { RouteParams, RouterLink } from "angular2/router";

import { IGNAThubBlob } from "gnat/reports";

import { highlight, highlightAda } from "../../ada-lang";
import { unescapeHTML } from "../../html-utils";
import { PathEncoder } from "../../path-encoder";
import { ReportService } from "../../services/report";

@Component({
    selector: "gnathub-blob",
    templateUrl: "app/components/gnathub-blob/gnathub-blob.html",
    directives: [ CORE_DIRECTIVES, RouterLink ],
    providers: [ ReportService ]
})
export class GNAThubBlob extends PathEncoder {
    private filename: string = null;
    private blob: IGNAThubBlob = null;

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
        this.reportService.GNAThubBlob(
            this.filename, (blob: IGNAThubBlob) => this.blob = blob);
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
        if (this.filename.endsWith(".ads") || this.filename.endsWith(".adb")) {
            return highlightAda(unescapeHTML(code));
        }
        if (this.filename.endsWith(".py")) {
            return highlight(unescapeHTML(code), "python");
        }
        if (this.filename.endsWith(".c") || this.filename.endsWith(".h")) {
            return highlight(unescapeHTML(code), "C");
        }
        return unescapeHTML(code);
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
