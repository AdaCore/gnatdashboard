import { Injectable } from "angular2/core";
import { Http, Response } from "angular2/http";
import { Observable } from "rxjs/Observable";

import { IGNATcoverageHunk, IGNATcoverageReport } from "gnat/reports";

import "rxjs/add/operator/map";

@Injectable()
export class ReportService {
    private reportRequest: Observable<Response> = null;
    private hunkRequest: Observable<Response> = null;

    private gnatcovReport: IGNATcoverageReport = null;
    private annotatedHunk: IGNATcoverageHunk = null;

    constructor (http: Http) {
        this.reportRequest = http.get("api/report/gnatcoverage").map(
            (res: Response) => res.json()
        );
        this.hunkRequest = http.get("api/report/source/zip_stream.adb").map(
            (res: Response) => res.json()
        );

        this.reportRequest.subscribe((report: IGNATcoverageReport) => {
            this.gnatcovReport = report;
            this.reportRequest = null;
        });
        this.hunkRequest.subscribe((hunk: IGNATcoverageHunk) => {
            this.annotatedHunk = hunk;
            this.hunkRequest = null;
        });
    }

    /**
     * Execute |callback| whenever the report is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public GNATcovReport(callback: (report: IGNATcoverageReport) => void): void
    {
        if (this.reportRequest) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.reportRequest.subscribe(() => callback(this.gnatcovReport));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.gnatcovReport);
        }
    }

    /**
     * Execute |callback| whenever the annotated source is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public AnnotatedSource(callback: (hunk: IGNATcoverageHunk) => void): void {
        if (this.hunkRequest) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.hunkRequest.subscribe(() => callback(this.annotatedHunk));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.annotatedHunk);
        }
    }
}
